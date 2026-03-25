```rust
pub(crate) type Foo = usize;

pub fn main() { }

pub fn bar() { }

impl Foo {
    fn main() { }
}
```

```molt
let input: Vis;
print(input);
```

```output
pub(crate)
pub
pub

```

