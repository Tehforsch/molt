# Match a let binding

```rust
fn main() {
    let x = 42;
}
```

```molt
let input: Stmt;
let { let $ident = $expr; }: Stmt = input;
print(ident);
```
