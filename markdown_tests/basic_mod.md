```rust
fn foo() -> usize {
    5 + 3
}
```

```molt
fn main(input: Expr) {
    let expr: Expr;
    let { $expr + 3 } = input;
    expr = { 10 };
}
```

