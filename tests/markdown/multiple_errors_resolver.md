```molt error
fn foo() {
    print(x);
}

fn bar() {
    print(y);
}
```

```output
error: undefined variable: 'x'
  ┌─ test input:2:11
  │
2 │     print(x);
  │           ^ not found in this scope

error: undefined variable: 'y'
  ┌─ test input:6:11
  │
6 │     print(y);
  │           ^ not found in this scope
```
