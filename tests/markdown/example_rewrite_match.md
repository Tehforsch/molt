```molt
fn main(input: Expr) {
    let name: Lit;
    let var: Ident;
    let arms: List<Arm>;
    let { match $var.fn1($name) { $arms } } = input;
    let k: Pat;
    input = { Ok(match $var.fn2($name) { $arms }) };
    for arm in arms {
        if let { Some(Foo::Value($k)) } = arm.pat {
            arm.pat = { Some($k) };
        }
    }
}

```

```rust
fn main() {
    match foo.fn1("name") {
        Some(Foo::Value(val)) => 1,
        None => 2,
    }
}
```

```rust reference
fn main() {
    Ok(match foo.fn2("name") { Some(val) => 1, None => 2, })
}
```
