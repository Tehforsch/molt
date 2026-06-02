Molt can bind a list of syntax nodes and then inspect or rewrite each element.

This rule matches an array behind a reference, prints the matched list, and
replaces each element with `5`:

```molt
fn main(input: Expr) {
    let list: List<Expr>;
    let { [$list] } = input;
    print(list);
    for item in list {
        print(item);
        item -> { 5 };
    }
}
```

Input Rust:

```rust
const X: &[usize] = &[1, 2, 3];
```

After running Molt:

```rust reference
const X: &[usize] = &[5, 5, 5];
```

Printed output:

```output
[1, 2, 3]
1
2
3
```
