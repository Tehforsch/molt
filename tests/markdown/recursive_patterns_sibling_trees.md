```molt
fn main(input: Expr) {
    let exprs: List<Expr>;
    let { [$exprs] } = input;
    input = { Some($input) };
    for expr in exprs {
        let inner_exprs: List<Expr>;
        let { [$inner_exprs] } = expr;
        expr = { Ok($expr) };
        for inner_expr in inner_exprs {
            inner_expr = { 0 };
        }
    }
}

```

```rust
fn main() {
    [[1, 2], [3, 4, 5]]
}
```

```rust reference
fn main() {
    Some([Ok([0, 0]), Ok([0, 0, 0])])
}
```
