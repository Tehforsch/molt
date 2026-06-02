Molt being aims to make it easy to express complex replacement logic.
For example, the following rule rewrites `foo();` statements inside functions named `do_something`, while leaving other `foo();` statements untouched.

```molt
fn main(input: Fn) {
    if let { do_something } = input.name {
        for stmt in input.stmts {
            if let { foo(); } = stmt {
                stmt -> { bar(); };
            }
        }
    }
}
```

Input Rust:

```rust
fn do_something() {
    foo();
    some_other_fn();
    let x = 3;
    foo();
}

fn some_other_fn() {
    foo();
}
```

After running Molt:

```rust reference
fn do_something() {
    bar();
    some_other_fn();
    let x = 3;
    bar();
}

fn some_other_fn() {
    foo();
}
```
