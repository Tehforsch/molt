const X: usize = 3;

fn foo() {}
pub fn foo() {}
unsafe fn foo() {}
async fn foo() {}
const fn foo() {}

impl Foo for Bar {
    fn foo() {}
    pub fn foo() {}
    unsafe fn foo() {}
    async fn foo() {}
    const fn foo() {}
}

impl Baz {
    fn foo() {}
    pub fn foo() {}
    unsafe fn foo() {}
    async fn foo() {}
    const fn foo() {}
}
