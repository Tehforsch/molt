```molt
let input: Fn;
let args: List<FnArg>;
let name: Ident;
let stmts: List<Stmt>;
let ret: Type;
let { fn $name($args) -> $ret { $stmts } } = input;
for arg in args {
    print(arg);
}
// print(name);
```

```rust
fn main() {
}

fn foo(a: A, b: B) -> Bar {
}
```

```output
a: A
b: B
```
