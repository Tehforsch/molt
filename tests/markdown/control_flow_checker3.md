```molt error
fn main() -> int {
    if true {
        return 5;
    } else if true {
        return 6;
    }
}
```

```output
error: type mismatch: expected `()`, found `int`
  ┌─ test input:1:4
  │
1 │ fn main() -> int {
  │    ^^^^ expects return type `int`
```
