## Int variable reused in pattern

When a variable declared as int is reused inside a pattern, the
typechecker allows it but parse_pats hits typeck_bug!() because
pattern variables must have Kind type.

```molt
let input: Expr;
let x: int = 5;
let { $x } = input;
```

```rust
fn foo() {
    1 + 2;
}
```

```rust reference
fn foo() {
    1 + 2;
}
```
