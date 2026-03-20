```molt error
fn main(input: Expr) {
    print(input)
    input = { 5 };
}
```

```output
error: expected semicolon
  ┌─ test input:3:5
  │
3 │     input = { 5 };
  │     ^^^^^ expected semicolon
  │
  = expected semicolon
```
