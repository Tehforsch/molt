```molt
let input: Fn;
if let { foo } = input.name {
    input.generics = { <T> };
}
```

```rust
fn foo() {
}

fn bar() {
}
```

```rust reference
fn foo<T>() {
}

fn bar() {
}
```
