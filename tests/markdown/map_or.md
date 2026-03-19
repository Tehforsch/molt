Automatically transform `map(...).unwrap_or(...)` into `map_or`:

```molt
let input: Expr;
let expr: Expr;
let closure: Expr;
let val: Expr;
let { $expr.map($closure).unwrap_or($val) } = input;
input = { $expr.map_or($val, $closure) };
```

```rust
fn test() -> i32 {
    let a = x.map(f).unwrap_or(0);
    let b = y.map(g).unwrap_or(1);
    a + b
}
```

```rust reference
fn test() -> i32 {
    let a = x.map_or(0, f);
    let b = y.map_or(1, g);
    a + b
}
```
