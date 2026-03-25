This is a pretty deranged test, but it tests a particular type of assignment,
namely one like `a = {$b}`. For simplicity `a` and `b` are the same here, but it
changes nothing. Normally, this would simply be written `a = b`, but writing it as
`a = {$b}` is technically legal and should certainly not crash the interpreter like
it has before.

```molt
fn main(input: Expr) {
    input = { $input };
}
```

```rust
fn foo() {
    bar
}
```

```rust reference
fn foo() {
    bar
}
```
