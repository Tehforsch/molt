# Molt

Molt is a syntax-aware search and replace tool for Rust. Molt is experimental software.

## Overview

Molt provides a simple pattern-matching language for the Rust programming language. 
It's particularly useful for mechanical refactorings, API migrations and pattern finding across large code bases.

## Example
Suppose you have a `main.rs` containing:
```rust
fn main() {
    let x = foo.get("key").insert("value");
    let y = bar.get_map().get(CustomEnum::Key).insert(10);
}
```

Also suppose that we want to use a different API that simplifies having to do `.get(key).insert(val)` with a single method call `insert_at_key(key, val)`. To do so, we write a molt file:

`merge_fns.molt`:
```rust
let key, val, expr: Expr;
let ident: Ident;

let old: Expr = $expr.get($key).insert($val);
let new: Expr = $expr.insert_at_key($key, $val);

transform old -> new;
```


Running `molt merge_fns.molt` in the rust project containing `main.rs` will result in:

`main.rs`, after:
```rust
fn main() {
    let x = foo.insert_at_key("key", "value");
    let y = bar.get_map().insert_at_key(CustomEnum::Key, 10);
}
```

## More Examples

### Simple Variable Renaming

```rust
let input: Ident = old_name;
let output: Ident = new_name;

transform input -> output;
```

### Type-aware Transformations (WIP)
A lot of times, simply matching for syntactic patterns is not enough, and we need to add type annotations:

```rust
let ident: Ident;
type ident = Foo; // Only match variables with type `Foo`
let old: Expr = $ident.foo();
let new: Expr = $ident.bar();
transform old -> new;
```

This works, but is currently implemented by asking rust-analyzer via a "hover request" to obtain the type of the identifier. Unsurprisngly, this turns out to be quite fragile since the info rust-analyzer returns is meant for human consumption. Therefore, this will have to be replaced by directly using rust-analyzer or rustc bindings.

### List matching (WIP)
Molt can match multiple items at the same syntactic levels at once:

```rust
let expr, val: Expr;
let pat: Pat;

let input: Expr = match $expr {$($arm_in)*};
let arm_in: Arm = { $pat => Ok($val) };
```

Here, the regex-like `$($arm_in)*` syntax means any amount of patterns that look like `arm_in`.

Simply matching patterns in this way works, but transforming them is not implemented currently. The goal is to be able to write something like
```rust
let expr, val: Expr;
let pat: Pat;

let input: Expr = match $expr { $($arm_in)* };
let arm_in: Arm = { $pat => Ok($val) };

let output: Expr = match Ok($expr { $($arm_out)* });
let arm_out: Arm = { $pat => $val };

transform input -> output;
```

The machinery to do this exists, but transforming the output requires carefully analyzing the "tree" of transformations and doing them in the right order, which is not implemented yet.

## Molt Syntax

A molt file consists of variable declarations and commands.

### Variable Declarations

Variables are declared using `let` statements with a kind annotation (see below for the different syntax kinds):

```rust
let var_name: Kind;
```

Variables can also be assigned a pattern to match against:

```rust
let var_name: Kind = <pattern>;
// Some patterns, such as most statements need to be enclosed in `{` `}` to be parsed correctly
let var_name: Kind = { <pattern> };
```

Multiple variables of the same type can be declared together.

```rust
let var1, var2, var3: Kind;
```

### Patterns

Patterns are written as Rust code that specifies the form of the pattern to match. The given Rust code needs to match the declared syntax kind.

```rust
let expr: Expr = 5; // Valid, `5` is an expression.
let expr: Ident = 5; // Invalid, `5` is not an identifier.
```

Other variables can be referenced within patterns by prepending the name of the variable with a `$`:
```rust
let expr: Expr;
let pattern: Expr = foo($expr);
```

Using a variable in this way can do one of two things:
1. If the variable is declared using an explicit pattern, the matcher will check that the concrete code we are looking at matches the pattern.
2. If the variable is declared without an explicit pattern, the matcher will bind that variable to the concrete syntactic expression in place of the variable. If the variable occurs multiple times, for example in 
```rust
let expr: Expr;
let pattern: Expr = $expr + $expr;
```
then the matcher will make sure that the second occurence matches the pattern bound to the variable by the first occurence.

#### List Patterns (WIP)

Use `$($var)*` syntax to match zero or more repetitions:

```rust
let arm: Arm;
let pattern: Expr = match expr { $($arm)* };
```

### Commands

Molt supports two main commands:

#### `match`

The `match` command searches for patterns in the code without modifying it:

```rust
let expr: Expr;
let pattern: Expr = foo($expr);
match pattern;
```

Running molt on this molt file searches for all occurrences of `pattern` and prints them.

In most cases, `match` can be omitted since the match variable can be inferred, so this would be equivalent:
```rust
let expr: Expr;
let pattern: Expr = foo($expr);
```

#### `transform`

The `transform` command searches for a pattern and replaces it with another:

```rust
let old: Expr = <old_pattern>;
let new: Expr = <new_pattern>;
transform old -> new;
```

### Special commands

#### `ignore`

Use `ignore` to ignore pattern matching for all occurences of a specific syntactic kind:

```rust
ignore Vis;
```

The motivation for this is that in some cases, specifying a pattern in its most general form can be tedious. For example, assume we want to match on a constant declaration:
```rust
let ident: Ident;
let expr: Expr;
let c: Stmt = {
    const $ident: Foo = Foo::new($expr);
};
```

Due to a mismatch in visibility, this pattern would not match on a constant declared like this:
```rust
pub const X: Foo = Foo::new(5);
```

We can fix this by matching on any kind of visibility explicitly:
```rust
let ident: Ident;
let expr: Expr;
let vis: Vis;
let c: Stmt = {
    $vis const $ident: Foo = Foo::new($expr);
};
```

However, this is easy to forget and tedious. `ignore` exists for this use case:
```rust
let ident: Ident;
let expr: Expr;
ignore Vis;
let c: Stmt = {
    const $ident: Foo = Foo::new($expr);
};
```

Visibilities can also be ignored/set to strict only for specific kinds:
```rust
strict Vis;
ignore Vis(Const, Trait); // Only ignore visibilities in constant declarations and traits.
```

#### `strict`

`strict` is the opposite of ignore. It has no use currently, since `strict` is currently the default for all visibilities, something that might be changed in the future.

## Syntax Kinds

The following kinds are currently supported.

### `Ident` - Identifiers

Matches any identifier.

```rust
let input: Ident = some_variable_name;
```

### `Lit` - Literals

Matches any literal value.

```rust
let lit: Lit = "foo"; // A String
let lit: Lit = 5; // A Number
```

### `Expr` - Expressions

Matches any Rust expression. Examples:

```rust
let e: Expr;
let pattern: Expr = $e.bla(); // Method calls
let pattern: Expr = $e.bar; // Field access

let e1, e2, e3: Expr;
let pattern: Expr = (-$e1) + ($e2 * $e3); // Unary, binary, parentheses

// If-else expressions
let cond, then, else_: Expr;
let pattern: Expr = {
    if $cond {
        $then
    } else {
        $else_
    }
};
```

### `Stmt` - Statements

Matches any Rust statement. Examples:

```rust
// Let bindings
let stmt: Stmt = { let foo = 3; };

// Nested statements
let stmt: Stmt;
let fn: Stmt = fn inner() { $stmt };
```

### `Type` - Types

Matches any type. Examples:

```rust
let ty: Type;
let pattern: Type = [$ty; 3]; // Arrays
let pattern: Type = ( $ty, $ty ); // Tuples
let pattern: Type = Foo<$ty>; // Generics
```

### `Pat` - Patterns

Matches any pattern in match expressions or destructuring. Examples:

```rust
let pat: Pat = Some(_);

let pat1, pat2: Pat;
let pattern: Pat = $pat1 | $pat2; // Or-patterns
let pattern: Pat = ($pat1, $pat2); // Tuples
```

### `Arm` - Match Arms

Matches match expression arms. Examples:

```rust
let pat: Pat;
let val: Expr;
let arm: Arm = { $pat => Ok($val) };
```

### `Field` - Struct Fields

Matches struct field definitions. Examples:

```rust
let ident: Ident;
let type: Type;
let x: Field = $ident: $type;
```

### `Vis` - Visibility Modifiers

Matches visibility modifiers. Examples:

```rust
let vis: Vis;
let ident: Ident;
let pattern: Item = $vis fn $ident() {};
```

### `Generics` - Generic Parameters

Matches generic parameter lists. Examples:

```rust
let ty: Type;
let generics: Generics = < $ty >;

let pattern: Item = fn process $generics (input: $ty) -> $ty {
    input.clone()
};
```

### `Item` - Top-Level Items

Matches top-level Rust items. Examples:

```rust
let vis: Vis;
let ident: Ident;
let pattern: Item = $vis fn $ident() {}; // Functions
let pattern: Item = $vis const $ident(): usize = 5; // Constants
let pattern: Item = { $vis mod $ident { } }; // Modules
```

## Attribution

The Rust parser for this project is adapted from [syn](https://github.com/dtolnay/syn).
