```molt error
fn main() {
}

fn foo() {
}
```

```output
warning: Variable is never used: foo
  ┌─ test input:4:4
  │
4 │ fn foo() {
  │    ^^^ declared here
```
