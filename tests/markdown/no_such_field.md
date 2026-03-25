```molt error
fn main(input: Fn) {
    print(input.foo);
}
```

```output
error: No field `foo` for type `Node(ItemFn | ImplItemFn)`. Available fields: generics, name, stmts, vis
```
