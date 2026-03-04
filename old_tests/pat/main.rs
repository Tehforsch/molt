fn pat_ident() {
    let x = 42;
    let mut y = 10;
    let ref z = 20;
    let ref mut w = 30;
}

fn pat_tuple() {
    let (a, b, c) = (1, 2, 3);
    let (x, (y, z)) = (10, (20, 30));
}

fn pat_struct() {
    struct Point { x: i32, y: i32 }
    let Point { x, y } = Point { x: 1, y: 2 };
    let Point { x: px, y: py } = Point { x: 1, y: 2 };
}

fn pat_tuple_struct() {
    struct Color(u8, u8, u8);
    let Color(r, g, b) = Color(255, 0, 0);
}

fn pat_slice() {
    let arr = [1, 2, 3, 4, 5];
    let [first, second, ..] = arr;
    let [.., last] = arr;
    let [first, .., last] = arr;
}

fn pat_reference() {
    let x = 42;
    let &y = &x;
    let &mut z = &mut 10;
}

fn pat_or() {
    let x = Some(42);
    match x {
        Some(1) | Some(2) | Some(3) => println!("small"),
        Some(n) => println!("other: {}", n),
        None => println!("none"),
    }
}

fn pat_paren() {
    match (1, 2) {
        (x) => println!("single: {}", x),
        (a, b) => println!("pair: {}, {}", a, b),
    }
}

fn pat_wild() {
    let x = 42;
    let _ = x;
    match x {
        42 => println!("forty-two"),
        _ => println!("other"),
    }
}

fn pat_range() {
    let x = 5;
    match x {
        1..=5 => println!("one to five"),
        6..10 => println!("six to nine"),
        _ => println!("other"),
    }
}

fn pat_lit() {
    let x = 42;
    match x {
        42 => println!("forty-two"),
        100 => println!("one hundred"),
        _ => println!("other"),
    }
}

fn pat_path() {
    enum Color { Red, Green, Blue }
    let c = Color::Red;
    match c {
        Color::Red => println!("red"),
        Color::Green => println!("green"),
        Color::Blue => println!("blue"),
    }
}

fn pat_type() {
    let x: i32 = 42;
}

fn nested_patterns() {
    struct Point { x: i32, y: i32 }
    let points = vec![Point { x: 1, y: 2 }, Point { x: 3, y: 4 }];
    
    for Point { x, y } in points {
        println!("Point({}, {})", x, y);
    }
    
    let nested = ((1, 2), (3, 4));
    let ((a, b), (c, d)) = nested;
}

fn subpatterns() {
    let x = Some(42);
    match x {
        Some(n @ 1..=10) => println!("small: {}", n),
        Some(n @ 11..=100) => println!("medium: {}", n),
        Some(n) => println!("large: {}", n),
        None => println!("none"),
    }
}
