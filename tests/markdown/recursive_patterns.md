```molt
fn main(input: Expr) {
    let exprs: List<Expr>;
    let { [$exprs] } = input;
    input = { Some($input) };
    for expr in exprs {
        expr = { 100 };
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
    Some([100, 100, 100])
}
```
