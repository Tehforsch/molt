## Pattern on LHS of assignment

Using a pattern expression on the left-hand side of an assignment
hits a todo!() in AssignmentLhs::from_expr.

```molt error
let x: Expr;
let input: Expr;
let { $x } = input;
{ 1 + 2 } = x;
```
