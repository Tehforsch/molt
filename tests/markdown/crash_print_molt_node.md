## Print a pattern-constructed node

Printing a pattern-constructed (Molt) node hits a todo!() in value_to_string
because it doesn't handle NodeRef::Molt variants.

```molt
let x: Expr = { 1 + 2 };
print(x);
```

```output
```
