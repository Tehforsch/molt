```molt error
let input: Expr;
input -> { 1 } if "foo";
```

```output
error: type mismatch: expected `str`, found `bool`
```
