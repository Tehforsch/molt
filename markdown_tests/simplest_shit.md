```molt
let input: Expr;
let foo: Ident;
let { $foo.bar() } = input;
input = { $foo };
```

```rust
fn main() {
    aaaa.bar();
    bbbb.bar();
    cccc.bar();
}
```

```rust reference
fn main() {
    aaaa;
    bbbb;
    cccc;
}
```
