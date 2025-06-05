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

    fn inner() {
        foo();
    }

    if cond1 {
        expr1
    } else if cond2 {
        expr2
    } else if cond3 {
        expr3
    } else if cond4 {
        expr4
    } else {
        expr5
    }
}
