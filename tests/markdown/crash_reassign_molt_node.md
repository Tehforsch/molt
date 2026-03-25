## Reassign pattern-constructed node

Reassigning a variable that holds a Molt node tries to create a
modification from the Molt node, hitting unreachable!() in
RealNodeRef::from_target.

```molt
let input: Fn;
let p: Fn = { fn bar() {} };
p = { fn baz() {} };
```

```rust
fn foo() {}
```

```rust reference
fn foo() {}
```
