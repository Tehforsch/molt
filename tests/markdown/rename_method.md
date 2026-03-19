```molt
fn main(input: Expr) {
    let var: Expr;
    let val: Expr;
    let { $var.old_name($val) } = input;
    input = { $var.new_name($val) };
}
```

```rust
fn test() -> i32 { register.old_name(some_str) }
```

```rust reference
fn test() -> i32 { register.new_name(some_str) }
```
