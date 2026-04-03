```molt error
fn main() {
    let a: int;
    let b: int;
}
```

```output
warning: Variable is never used: a
  ┌─ test input:2:9
  │
2 │     let a: int;
  │         ^ declared here

warning: Variable is never used: b
  ┌─ test input:3:9
  │
3 │     let b: int;
  │         ^ declared here
```
