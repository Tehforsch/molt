```molt error
fn main() {
    let var1: int;
    print(var1);
}
```

```output
error: variable not initialized: 'var1'
  ┌─ test input:3:11
  │
2 │     let var1: int;
  │         ---- declared here without a value
3 │     print(var1);
  │           ^^^^ used before initialization
```
