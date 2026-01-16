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

## Attribution

The Rust parser for this project is adapted from [syn](https://github.com/dtolnay/syn).
