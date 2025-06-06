struct Foo {
    bar: Bar<Baz>,
}

type Result<T, E=Default> = std::result::Result<T, E>;

pub const X: (Foo, Bar, Baz) = bar;

impl<'a, const x: usize, T> Foo<'a, T> {
}

fn foo<T>(x: T) where for<'a> &'a Foo: Trait {
    let closure = |x: usize, y: Foo<Bar>| {
    };
}

impl Foo for Bar {
}

static FOO: Baz = 50;

trait Foo {
    const CONST: usize = 5;
}
