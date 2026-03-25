```molt
fn main(input: Expr) {
    let exprs: List<Expr>;
    let { [$exprs] } = input;
    input = { Some($input) };
    for expr in exprs {
        expr = { Box::new($expr) };
    }
}

```

```rust
fn main() {
    [1, 2, 3]
}
```

```rust reference
fn main() {
    Some([Box::new(1), Box::new(2), Box::new(3)])
}
```
