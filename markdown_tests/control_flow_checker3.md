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
error: Type mismatch. Expected Unit, found Int
```
