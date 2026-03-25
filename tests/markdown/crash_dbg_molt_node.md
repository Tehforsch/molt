## Dbg a pattern-constructed node

Using dbg on a pattern-constructed node - the dbg builtin tries to emit
a diagnostic using the molt_id, but the context doesn't have a real source
for it in the right way.

```molt
let x: Expr = { 1 + 2 };
dbg(x);
```

```output
```
