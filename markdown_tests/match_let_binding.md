```rust
fn main() {
    let x = 42;
}
```

```molt
let input: Stmt;
let ident: Ident;
let expr: Expr;
let { let $ident = $expr; }: Stmt = input;
print(ident);
```
