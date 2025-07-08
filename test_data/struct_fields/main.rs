struct Point {
    x: f64,
    y: f64,
}

struct Person {
    name: String,
    age: u32,
    email: String,
    active: bool,
}

struct Config {
    host: String,
    port: u16,
    timeout: Duration,
    retry_count: u32,
    debug: bool,
}

// Tuple structs
struct Color(u8, u8, u8);
struct Wrapper(String);
struct Multi(i32, String, bool, Vec<i32>);
