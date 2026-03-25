## Field access on a pattern-constructed node

Accessing a field on a Fn-typed pattern node hits todo!() in Value::get_type
because it doesn't handle NodeRef::Molt variants.

```molt
let input: Fn;
let p: Fn = { fn bar() {} };
print(p.name);
```

```rust
fn foo() {}
```

```rust reference
fn foo() {}
```
