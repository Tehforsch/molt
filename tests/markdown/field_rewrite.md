```molt
fn main(input: Fn) {
    input.name = { bar };
    input.vis = { pub };
}
```

```rust
fn main() {
    println!("hello");
}

impl Foo {
    fn foo(input: Fn) {
    }
}
```

```rust reference
pub fn bar() {
    println!("hello");
}

impl Foo {
    pub fn bar(input: Fn) {
    }
}
```
