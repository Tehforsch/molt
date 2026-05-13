# fn args list

Match fn args as a list variable and print each one:

```molt
fn main(input: Fn) {
    let args: List<FnArg>;
    let name: Ident;
    let stmts: List<Stmt>;
    let {
        fn $name($args) {
            $stmts
        }
    } = input;
    print(name);
    for arg in args {
        print(arg);
    }
}
```

```rust
fn foo(a: i32, b: String) {
    println!("{}", a);
}

fn bar() {
}
```

```output
foo
a: i32
b: String
bar
```
