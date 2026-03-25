```molt
fn rewrite_type(inner: Type) {
    if let { &Handle<Image> } = inner {
        inner = { &Foo };
    }
}

fn main(input: Type) {
    let inner_tys: List<Type>;
    let ty: Type;
    if let { Query<($inner_tys)> } = input {
        for ty in inner_tys {
            rewrite_type(ty);
        }
    } else if let { Query<$ty> } = input {
        rewrite_type(ty);
    }
}

```

```rust
fn foo(q: Query<(Ty1, &Handle<Image>, Ty2)>) { }
fn foo(q: Query<(&Handle<Image>, )>) { }
fn foo(q: Query<&Handle<Image>>) { }
fn foo(q: Query<(Ty1, Handle<Image>, Ty2)>) { }
```

```rust reference
fn foo(q: Query<(Ty1, &Foo, Ty2)>) { }
fn foo(q: Query<(&Foo, )>) { }
fn foo(q: Query<&Foo>) { }
fn foo(q: Query<(Ty1, Handle<Image>, Ty2)>) { }
```
