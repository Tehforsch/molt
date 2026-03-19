```rust
fn foo() {
    let x = 5 + 3;
}
```

```molt
fn main(input: Expr) {
    let expr: Expr;
    let { $expr + 3 } = input;
    print(expr);
}
```

```output
5
```
