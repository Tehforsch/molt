## List variable with non-Kind inner type in pattern

When a List variable whose inner type is non-Kind (e.g., List<int>)
is used in a pattern, parse_pats hits typeck_ensures on the inner type.

```molt
let input: Expr;
let x: List<int> = [1, 2, 3];
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
