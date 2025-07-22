enum MyResult<T, E> {
    Ok(T),
    Err(E),
}

enum MyOption<T> {
    Some(T),
    None,
}

struct Vec<T> {
    data: *const T,
    len: usize,
}

struct Point<T> {
    x: T,
    y: T,
}

type MyResult2<T> = Result<T, MyError>;

trait MyIterator<Item> {
    fn next(&mut self) -> Option<Item>;
}

trait MyClone<T = Self> {
    fn clone(&self) -> T;
}

impl<T> MyClone<T> for Vec<T> 
where 
    T: Copy
{
    fn clone(&self) -> T {
        todo!()
    }
}

impl<T> MyIterator<T> for Vec<T> {
    fn next(&mut self) -> Option<T> {
        todo!()
    }
}

fn process<T: Clone>(input: T) -> T {
    input.clone()
}

fn generic_fn<T, U>(a: T, _b: U) -> T 
where 
    T: Copy
{
    a
}

struct MyError;
