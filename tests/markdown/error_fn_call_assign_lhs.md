```molt error
fn foo() {}
foo() = 1;
```

```output
error: Invalid left hand side of assignment
  ┌─ test input:2:1
  │
2 │ foo() = 1;
  │ ^^^^^ here
```
