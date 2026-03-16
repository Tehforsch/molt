```rust
fn main() {
    hello.foo(3 + 5);
}
```

```molt
let input: Stmt;
let ident: Ident;
let expr: Expr;
let { $ident.foo($expr); }: Stmt = input;
print(ident);
```

```output
hello
```
