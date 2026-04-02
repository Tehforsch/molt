```molt error
let input: Expr;
let x: Expr;
let { $x } = input;
let { $x };
```

TODO: Improve this error message, add span.
```output
error: Let statement with pattern on left-hand side has no right-hand side.
```
