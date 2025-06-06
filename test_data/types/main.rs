struct Foo {
    bar: Bar<Baz>,
}

type Result<T, E=Default> = std::result::Result<T, E>;

pub const x: (Foo, Bar, Baz) = bar;

impl<'a, T> Foo<'a, T> {
}
