```molt
fn rewrite(e1: Expr, e2: Expr) -> Expr {
    if let { 2 } = e2 {
        { foo() }
    } else if let { 3 } = e2 {
        { bar() }
    } else {
        { baz() }
    }
}

fn main(input: Item) {
    let stmts: List<Stmt>;
    let { fn foo() { $stmts } } = input;
    for stmt in stmts {
        if let { $e1 + $e2; } = stmt {
            e1 = rewrite(e1, e2);
        }
    }
}
```

```rust
fn foo() {
    1 + 2;
    1 + 3;
    1 + 4;
}
```

```rust reference
fn foo() {
    foo() + 2;
    bar() + 3;
    baz() + 4;
}
```
