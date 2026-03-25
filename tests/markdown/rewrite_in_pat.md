```molt
let input: Fn;
let stmts: List<Stmt>;
let name: Ident;
let { 
    fn $name() {
        $stmts
    }
} = input;
name = { bar };
print(name);
```

```rust
fn main() {
    println!("hello");
}
```

```rust reference
fn bar() {
    println!("hello");
}
```

```output
main
```
