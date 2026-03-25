## Let with pattern LHS but no rhs

A let statement with a pattern LHS but no rhs expression hits todo!()
in the interpreter.

```molt error
let input: Expr;
let x: Expr;
let { $x } = input;
let { $x };
```
