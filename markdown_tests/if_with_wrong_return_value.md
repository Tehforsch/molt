```molt error
fn main() -> int {
    if true {
        return "hello";
    }
    return 5;
}
```

```output
error: type mismatch: expected `int`, found `str`
```
