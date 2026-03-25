```molt
fn main(input: Expr) {
    let arms: List<Arm>;
    let expr: Expr;
    let val: Expr;
    let { match $expr { $arms } } = input;
    // TODO: Introduce matches! and closures and `.all` 
    // on lists to make this prettier.
    let all_match = true;
    for arm in arms { 
        if let { Ok($val) } = arm.body { 
        } else {
            all_match = false; 
        }
    }
    if all_match {
        for arm in arms {
            let { Ok($val) } = arm.body;
            arm.body = val;
        }
        input = { Ok($input) };
    }
}
```

```rust
fn main() {
    match foo() {
        pat1 => Ok(val1),
        pat2 => Ok(val2),
        pat3 => Ok(val3),
    };

    match foo() {
        pat1 => Ok(val1),
        pat2 => val2,
        pat3 => Ok(val3),
    };
}
```

```rust reference
fn main() {
    Ok(match foo() {
        pat1 => val1,
        pat2 => val2,
        pat3 => val3,
    });

    match foo() {
        pat1 => Ok(val1),
        pat2 => val2,
        pat3 => Ok(val3),
    };
}
```
