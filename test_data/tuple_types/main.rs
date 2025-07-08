fn foo() -> (i32, String) {
    todo!()
}

fn bar() -> (i32, String, bool) {
    todo!()
}

fn baz() -> (A, B, C, D) {
    todo!()
}

struct Point(f64, f64);

type Coords = (f64, f64, f64);
type Mixed = (i32, &str, bool, Vec<i32>);
