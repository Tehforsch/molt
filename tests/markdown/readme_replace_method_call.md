Suppose we have a `main.rs` containing:

```rust
fn main() {
    let x = foo.get("key").insert("value");
    let y = bar.get_map().get(CustomEnum::Key).insert(10);
}
```

Suppose we want to replace `.get(key).insert(val)` with a single method call `insert_at_key(key, val)`.
We write the following molt file:

`merge_fns.molt`:

```molt
fn main(input: Expr) {
    let key: Expr;
    let val: Expr;
    let expr: Expr;
    let { $expr.get($key).insert($val) } = input;
    input -> { $expr.insert_at_key($key, $val) };
}
```

Running `molt merge_fns.molt` in the rust project will result in:

`main.rs`, after:

```rust reference
fn main() {
    let x = foo.insert_at_key("key", "value");
    let y = bar.get_map().insert_at_key(CustomEnum::Key, 10);
}
```
