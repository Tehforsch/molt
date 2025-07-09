fn main() {
    let x = 42;
    let y = 3.14;
    let z = "hello";
    let a = 'a';
    let b = true;
    let c = false;
    let d = b"bytes";
    let e = r"raw string";
    let f = 0x1a;
    let g = 0o777;
    let h = 0b1010;
    let i = 42_u32;
    let j = 3.14_f64;
    
    // In expressions
    foo(123);
    bar("literal");
    baz(true);
    
    // In match patterns
    match value {
        42 => println!("forty-two"),
        "hello" => println!("greeting"),
        true => println!("truth"),
        _ => println!("other"),
    }
    
    // In arrays
    let arr = [1, 2, 3];
    let str_arr = ["a", "b", "c"];
    
    // In attribute values
    #[cfg(feature = "test")]
    fn test_func() {}
    
    /// Docstr
    /// Continued docstr
    #[repr(align(16))]
    struct Aligned;
}
