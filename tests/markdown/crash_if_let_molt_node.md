## If-let matching on pattern-constructed node

Using if-let to match on a pattern-constructed node crashes because
eval_let_lhs_pat expects a Real node via typeck_ensures.

```molt
let p: Expr = { 1 + 2 };
let x: Expr;
let y: Expr;
if let { $x + $y } = p {
    print(x);
}
```

```output
```
