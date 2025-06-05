fn foo() {
    (x+1);
    foo.0.0.0.0;
    foo.0.0.0.0.0;
    match x {
        foo..bar => {},
        ..bar => {},
        .. => {},
        'a' => {},
    }

    let foo = bar.baz().bla().qux();
}
