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

impl Foo {
    const CONST: usize = 5;
    type Result = Baz;
}

fn foo(self: Box<Self>) {
}

const X: [usize; 3] = [1, 2, 3];

fn foo<T>(t: T) where <T as Trait>::Item: Trait2 {
}

const SLICE: [usize] = [1, 2, 3];

fn foo(x: fn(usize) -> String) {
}
