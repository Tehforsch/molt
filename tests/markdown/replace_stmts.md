```molt
fn main(input: Fn) {
    if let { main } = input.name {
        for stmt in input.stmts {
            if let {foo();} = stmt {
                stmt = { bar(); };
            }
        }
    }
}
```

```rust
fn main() {
    foo();
    some_other_fn();
    let x = 3;
    foo();
}

fn some_other_fn() {
    foo();
}
```

```rust reference
fn main() {
    bar();
    some_other_fn();
    let x = 3;
    bar();
}

fn some_other_fn() {
    foo();
}
```
