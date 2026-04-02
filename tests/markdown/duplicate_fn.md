```molt error
fn foo() { }

fn foo() { }
```

```output
error: function defined twice: 'foo'
  ┌─ test input:3:4
  │
1 │ fn foo() { }
  │    --- first defined here
2 │ 
3 │ fn foo() { }
  │    ^^^ duplicate definition
```
