```molt
let input: Expr;
let e: Expr;
let { foo(!$e) } = input;
input = { bar($e) };
```

```rust
fn test() {
    let a = foo(!x);
    let b = foo(!(a + b));
}
```

```rust reference
fn test() {
    let a = bar(x);
    let b = bar((a + b));
}
```

