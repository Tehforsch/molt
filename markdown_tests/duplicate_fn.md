```molt error
fn foo() { }

fn foo() { }
```

```output
error: Function defined twice: 'Ident { sym: foo, span: bytes(18..21) }'
```
