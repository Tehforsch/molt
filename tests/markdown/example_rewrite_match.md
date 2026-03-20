```molt
fn main(input: Expr) {
    let name: Lit;
    let var: Ident;
    let arms: List<Arm>;
    let { match $var.named($name) { $arms } } = input;
    let k: Pat;
    let result: Expr;
    for arm in arms {
        if let { Some(Foo::Value($k)) => $result } = arm {
            arm = { Some($k) => $result, };
        }
    }
}

```

```rust
fn main() {
    match foo.named("name") {
        Some(Foo::Value(val)) => 1,
        None => 2,
    }
}
```

```rust reference
fn main() {
    match foo.named("name") {
        Some(val) => 1,
        None => 2,
    }
}
```
