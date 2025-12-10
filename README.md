# Molt

Molt is a syntax-aware search and replace tool for Rust. Molt is experimental software.

## Overview

Molt provides a simple pattern-matching language for the Rust programming language. 
It's particularly useful for mechanical refactorings, API migrations and pattern finding across large code bases.

## Example
Suppose you have a `main.rs` containing:
```rust
fn main() {
    let x = foo.get_map().get("key").insert("value");
    let y = bar.get_map().get(CustomEnum::Key).insert(10);
    let z = baz.get(3).insert("my_string");
}
```

Also suppose that we want to use a different API that simplifies having to do `.get(key).insert(val)` with a single method call `insert_at_key(key, val)`, but we only want to do so for those maps that were obtained via `get_map()`. To do so, we write a molt file:

`merge_fns.molt`:
```rust
let key, val: Expr;
let ident: Ident;

let expr: Expr = $ident.get_map();

let old: Expr = $expr.get($key).insert($val);
let new: Expr = $expr.insert_at_key($key, $val);

mod old -> new;
```


Running `molt merge_fns.molt` in the rust project containing `main.rs` will result in:

`main.rs`, after:
```rust
fn main() {
    let x = foo.get_map().insert_at_key("key", "value");
    let y = bar.get_map().insert_at_key(CustomEnum::Key, 10);
    let z = baz.get(3).insert("my_string"); // baz does not match `$ident.get_map()`, so it did not get rewritten
}
```

## Attribution

The Rust parser for this project is adapted from [syn](https://github.com/dtolnay/syn).
