```molt
let input: Expr;
let expr: Expr;
let { $expr + 3 } = input;
expr = { 10 };
```

```rust
fn foo() -> usize {
    5 + 3;
    5 + 3;
    5 + 3;
    5 + 3;
}
```

```rust reference
fn foo() -> usize {
    10 + 3;
    10 + 3;
    10 + 3;
    10 + 3;
}
```

