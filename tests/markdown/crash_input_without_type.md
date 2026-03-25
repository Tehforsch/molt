## Input variable without type annotation

Declaring input without a type annotation causes unwrap on None
in add_implicit_main.

```molt error
let input;
print(input);
```
