Swap function arguments:
```molt
fn main(input: Expr) {
    let a: Expr;
    let b: Expr;
    let { foo($a, $b) } = input;
    input = { foo($b, $a) };
}
```

```rust
fn test() -> i32 { foo(1, 2) }
```

```rust reference
fn test() -> i32 { foo(2, 1) }
```
