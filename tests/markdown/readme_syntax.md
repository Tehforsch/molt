## Molt Syntax

A Molt file is a small program that runs against Rust syntax nodes.

### Program entry point

Molts entry point is the main function, which takes a syntax node as an argument

```molt check_only
fn main(input: Expr) { }
```

### Alternative program entry point
Alternatively, the main function can be omitted, but a `input` variable needs to be present and its type needs to be annotated:

```molt check_only
let input: Expr;
```

### Pattern matching with `let`
Patterns are written as Rust syntax inside `{ }`. Patterns can be matched/destructured with the `let` statement, which takes a pattern on the left-hand side and a syntax node expression on the right-hand side:

```molt check_only
let input: Expr;
let x: Expr;
let y: Expr;
let { $x + $y } = input;
```

The `let` statement checks for matches against the right hand side and binds the LHS variables to the matched nodes if a match was found. If no match was found, the `let` statement implicitly returns from the current function, so none of the checks below the let statement will take place if a single `let` fails.


### Pattern matching with `if let`

```molt check_only
let input: Expr;
let value: Expr;
if let { Some($value) } = input {
    print(value);
}
```

### Binding variables in patterns

Variables inside Rust patterns are referenced with `$name`. If the variable has
not been declared yet, a type annotation which declares its syntactic kind needs
to be present before the match:

```molt check_only
let input: Expr;
let receiver: Expr;
let key: Expr;
if let { $receiver.get($key) } = input {
    print(key);
}
```

### Rewrites

Use `target -> { replacement };` to replace a matched syntax node:

```molt check_only
let input: Expr;
let arg: Expr;
if let { old_name($arg) } = input {
    input -> { new_name($arg) };
}
```

### Field access

Some syntactic nodes offer their sub-nodes via field access:

```molt check_only
let input: Fn;
if let { foo } = input.name {
    input.generics -> { <T> };
}
```

### Lists and `for` loops

Use `List<T>` to bind multiple syntax nodes at the same level. 
The `for` loop iterates over items of a list.

```molt check_only
fn main(input: Fn) {
    let args: List<FnArg>;
    let name: Ident;
    let stmts: List<Stmt>;
    let {
        fn $name($args) {
            $stmts
        }
    } = input;

    for arg in args {
        print(arg);
    }
}
```

### `print`

Molt currently has a small set of builtin functions.

`print` simply prints the given variable / syntax node:

```molt
let input: Fn;
print(input.name);
```

```rust
fn example() {}
```

```output
example
```

### `dbg`
The `dbg` builtin prints the given variable along with some surrounding context

```molt
let input: Fn;
dbg(input.name);
```

```rust
fn example() {}
```

```output
note: 
  ┌─ test input:1:4
  │
1 │ fn example() {}
  │    ^^^^^^^
```

## Syntactic kinds

The following kinds are currently supported.

### `Ident` - Identifiers

Matches any identifier.

```molt
let input: Ident;
print(input);
```

```rust
fn main() {
    let x = 3;
}
```

```output
main
x
```


### `Lit` - Literals

Matches any literal value.

```molt
let input: Lit;
print(input)
```

```rust
fn main() {
    let x = 3;
    let y = "foo";
}
```

```output
3
"foo"
```

### `Expr` - Expressions

Matches any Rust expression. Note that this will often recursively expressions along with their subexpressions:

```molt
let input: Expr;
print(input);
```

```rust
fn main() {
    let x = 3 + 3;
}
```

```output
3
3
3 + 3
```

### `Stmt` - Statements

Matches any Rust statement. Examples:

```molt
let input: Stmt;
if let { let foo = 3; } = input {
    input -> { let foo = 4; };
}
```

```rust
fn main() {
    let foo = 3;
    let bar = 5;
}
```

```rust reference
fn main() {
    let foo = 4;
    let bar = 5;
}
```

### `Type` - Types

Matches any type. Examples:

```molt check_only
fn main(input: Type) {
    let ty: Type;
    if let { Vec<$ty> } = input {
        input -> { Box<[$ty]> };
    }
}
```

### `Pat` - Patterns

Matches any rust pattern in match expressions or destructuring. Examples:

```molt
let input: Pat;
let pat: Pat;
if let { Some($pat) } = input {
    input -> { $pat };
}
```

```rust
fn main() {
    if let Some("bar") = foo() {
        println!("baz");
    }
}
```

```rust reference
fn main() {
    if let "bar" = foo() {
        println!("baz");
    }
}
```

### `Arm` - Match Arms

Matches match expression arms. Examples:

```molt
let input: Arm;
let pat: Pat;
let val: Expr;
if let { $pat => Ok($val) } = input {
    input.body -> { $val };
}
```

```rust
fn main() {
    match x {
        MyEnum::A => Ok(1),
        MyEnum::B => Ok(2),
        MyEnum::C => Ok(3),
    }
}
```

```rust reference
fn main() {
    match x {
        MyEnum::A => 1,
        MyEnum::B => 2,
        MyEnum::C => 3,
    }
}
```

### `Field` - Struct Fields

Matches struct field definitions. Examples:

```molt
let input: Field;
print(input);
```

```rust
struct Foo {
    a: A,
    b: B,
    c: C,
}
```

```output
a: A
b: B
c: C
```

### `Vis` - Visibility Modifiers

Matches visibility modifiers. Examples:

```molt
let input: Vis;
if let { pub } = input {
    input -> { pub(crate) };
}
```

```rust
pub fn foo() { }

pub(crate) fn bar() { }
```

```rust reference
pub(crate) fn foo() { }

pub(crate) fn bar() { }
```

### `Generics` - Generic Parameters

Matches generic parameter lists. Examples:

```molt
let input: Generics;
print(input);
```

```rust
fn foo<A, B, C: std::fmt::Debug>() { }
```

```output
<A, B, C: std::fmt::Debug>
```

### `Item` - Top-Level Items

Matches top-level Rust items such as functions, trait definitions, use statements, constants. Examples:

```molt
let input: Item;
print(input);
```

```rust
const X: usize = 3;

fn foo() { }
```

```output
const X: usize = 3;
fn foo() { }
```
