```molt error
fn _foo() { }

fn _foo() { }
```

```output
error: function defined twice: '_foo'
  ┌─ test input:3:4
  │
1 │ fn _foo() { }
  │    ---- first defined here
2 │ 
3 │ fn _foo() { }
  │    ^^^^ duplicate definition
```
