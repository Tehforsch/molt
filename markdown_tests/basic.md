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
# Match statement
```rust
fn main() {
    hello.foo(3 + 5);
}
```

```molt
let input: Stmt;
let { $ident.foo($expr); }: Stmt = input;
print(ident);
```

# Literals
```molt
print(5);
```
