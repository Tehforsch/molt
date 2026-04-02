Use non-syntactic variable in pattern.

```molt error
let input: Expr;
let x: int = 1;
let { $x } = input;
let y: List<str> = ["a", "b"];
let { $y } = input;
```

```output
error: only variables with syntactic type are allowed in patterns
  ┌─ test input:3:7
  │
2 │ let x: int = 1;
  │     - has type int
3 │ let { $x } = input;
  │       ^^ variable used in pattern here

error: only variables with syntactic type are allowed in patterns
  ┌─ test input:5:7
  │
4 │ let y: List<str> = ["a", "b"];
  │     - has type List<str>
5 │ let { $y } = input;
  │       ^^ variable used in pattern here
```
