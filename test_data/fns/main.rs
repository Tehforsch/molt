const X: usize = 3;

fn foo() {}
unsafe fn foo() {}
async fn foo() {}

impl Foo for Bar {
    fn foo() {}
    unsafe fn foo() {}
    async fn foo() {}
}

impl Baz {
    fn foo() {}
    unsafe fn foo() {}
    async fn foo() {}
}
