# fn args single

Match a function with exactly one typed arg and rewrite its type:

```molt
fn main(input: Fn) {
    let arg_name: Pat;
    let arg_ty: Type;
    let stmts: List<Stmt>;
    let name: Ident;
    if let {
        fn $name($arg_name: $arg_ty) {
            $stmts
        }
    } = input {
        if let { i32 } = arg_ty {
            arg_ty -> { u64 };
        }
    }
}
```

```rust
fn foo(x: i32) {
    println!("{}", x);
}

fn bar(a: u8, b: u8) {
}

fn baz() {
}
```

```rust reference
fn foo(x: u64) {
    println!("{}", x);
}

fn bar(a: u8, b: u8) {
}

fn baz() {
}
```
