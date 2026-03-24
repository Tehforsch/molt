```molt error
fn main() -> Foo {
}
```

```output
error: expected one of: `Arm`, `Expr`, `Field`, `Ident`, `Item`, `ImplItem`, `Lit`, `Pat`, `Stmt`, `Type`, `Vis`, `Generics`, `Fn` or primitives `bool`, `int`, `str`, `List`
  ┌─ test input:1:14
  │
1 │ fn main() -> Foo {
  │              ^^^ expected one of: `Arm`, `Expr`, `Field`, `Ident`, `Item`, `ImplItem`, `Lit`, `Pat`, `Stmt`, `Type`, `Vis`, `Generics`, `Fn` or primitives `bool`, `int`, `str`, `List`
  │
  = expected one of: `Arm`, `Expr`, `Field`, `Ident`, `Item`, `ImplItem`, `Lit`, `Pat`, `Stmt`, `Type`, `Vis`, `Generics`, `Fn` or primitives `bool`, `int`, `str`, `List`
```
