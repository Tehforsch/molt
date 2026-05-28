```molt
let input: Expr;
input -> { 1 } if true;
input -> { 2 } if false;
```

```rust
fn main() {
    let x = 3;
    let x = 4;
}
```

```rust reference
fn main() {
    let x = 1;
    let x = 1;
}
```
