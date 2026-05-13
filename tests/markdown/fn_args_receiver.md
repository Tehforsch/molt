# fn args receiver

Match methods with a `&self` receiver and print their names:

```molt
fn main(input: Fn) {
    let name: Ident;
    let stmts: List<Stmt>;
    if let {
        fn $name(&self) {
            $stmts
        }
    } = input {
        print(name);
    }
}
```

```rust
impl Foo {
    fn bar(&self) {
        println!("hello");
    }

    fn baz(x: i32) {
    }

    fn qux(&mut self) {
    }
}
```

```output
bar
```
