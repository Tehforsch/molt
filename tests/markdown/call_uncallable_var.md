```molt error
let x = 3;
x();
```

```output
error: type mismatch: expected `fn() -> var`, found `int`
  ┌─ test input:1:5
  │
1 │ let x = 3;
  │     - has type `int`
```
