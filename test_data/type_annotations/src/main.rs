struct Config {
    foo: bool,
    timeout: usize,
}

impl Config {
    fn new(foo: bool, timeout: usize) -> Self {
        Config { foo, timeout }
    }
}

fn main() {
    let foo: i32 = 42;
    let foo: usize = foo as usize;
    let bar: String = "hello".to_string();
    let baz: i32 = 10;
    let config = Config::new(false, 5);
    println!("{} {}", config.foo, config.timeout);

    println!("foo: {}, bar: {}, baz: {}", foo, bar, baz);
}
