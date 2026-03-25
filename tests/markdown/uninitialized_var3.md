```molt error
fn do_something(e: Expr) { }

fn main(input: Expr) {
    let var1: Expr;
    let var2: Expr;
    if let { Ok($var1) } = input {
        do_something(var1);
    } else if let { Some($var2) } = input {
        do_something(var1);
    }
}
```

```output
error: Variable not initialized: 'var1'
```
