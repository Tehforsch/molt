```molt error
fn main(input: Fn) {
    print(input.foo);
}
```

```output
error: no field `foo` for type `Node(ItemFn | ImplItemFn)`. Available fields: generics, name, stmts, vis
  ┌─ test input:2:17
  │
2 │     print(input.foo);
  │                 ^^^ unknown field
```
