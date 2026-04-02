```molt error
let x: Expr;
let input: Expr;
let { $x } = input;
{ 1 + 2 } = x;
```

```output
error: Invalid left hand side of assignment
  ┌─ test input:4:1
  │
4 │ { 1 + 2 } = x;
  │ ^^^^^^^^^ here
```
