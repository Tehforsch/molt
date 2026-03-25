## Function call on LHS of assignment

Using a function call on the left-hand side of an assignment
hits a todo!() in AssignmentLhs::from_expr.

```molt error
fn foo() {}
foo() = 1;
```
