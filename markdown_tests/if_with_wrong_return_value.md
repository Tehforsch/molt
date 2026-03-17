```molt error
fn main() -> int {
    if true {
        return "hello";
    }
    return 5;
}
```

```output
error: Type mismatch. Expected Int, found Str
```
