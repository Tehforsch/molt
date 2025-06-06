struct Foo {
    bar: Bar<Baz>,
}

pub const x: (Foo, Bar, Baz) = bar;

impl<'a, T> Foo<'a, T> {
}
