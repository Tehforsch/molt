```molt error
fn foo() -> str {
    return 5;
}

fn bar() -> int {
    return "hello";
}
```

```output
warning: Variable is never used: foo
  ┌─ test input:1:4
  │
1 │ fn foo() -> str {
  │    ^^^ declared here

warning: Variable is never used: bar
  ┌─ test input:5:4
  │
5 │ fn bar() -> int {
  │    ^^^ declared here

error: type mismatch: expected `str`, found `int`

error: type mismatch: expected `int`, found `str`
```
