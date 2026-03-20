```rust
const X: &[usize] = &[1, 2, 3];
```

```rust reference
const X: &[usize] = &[5, 5, 5];
```

```molt
fn main(input: Expr) {
    let list: List<Expr>;
    let { &[$list] } = input;
    print(list);
    for item in list {
        print(item);
        item = { 5 };
    }
}
```

```output
[1, 2, 3]
1
2
3
```
