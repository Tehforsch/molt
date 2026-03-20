```rust
const X: &[usize] = &[1, 2, 3];
```

```molt
fn main(input: Expr) {
    let list: List<Expr>;
    let { &[$list] } = input;
    print(list);
}
```

```output
[1, 2, 3]
```
