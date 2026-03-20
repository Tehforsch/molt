```molt
let input: Expr;
let e1: Expr;
let e2: Expr;
let { $e1 + $e2 } = input;
if let { 2 } = e2 {
    e1 = { foo() };
} else if let { 3 } = e2 {
    e1 = { bar() };
} else {
    e1 = { baz() };
}
```

```rust
fn foo() {
    1 + 2;
    1 + 3;
    1 + 4;
}
```

```rust reference
fn foo() {
    foo() + 2;
    bar() + 3;
    baz() + 4;
}
```
