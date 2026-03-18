```molt error
fn main() -> int {
    if true {
        5
    } else {
        "hi"
    }
}
```

```output
error: type mismatch: expected `int`, found `str`
```
