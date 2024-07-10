#[derive(Debug)]
struct Rectangle {
    length: u32,
    width: u32
}

impl Rectangle {
    fn area(&self) -> u32 {
        self.length * self.width
    }

    fn can_hold(&self, other: &Rectangle) -> bool {
        self.length > other.length && self.width > other.width
    }

    fn square(size: u32) -> Rectangle {
        Rectangle { length: size, width: size }
    }
}

fn first_word(s: &String) -> &str {
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[..i];
        }
    }

    &s[..]
}

fn main() {
    let s = String::from("hello world");
    let index = first_word(&s);

    println!("Hello, world! {index}");

    let a = [1, 2, 3, 4, 5];
    let slice = &a[1..=3];

    for (_, &item) in slice.iter().enumerate() {
        println!("{}", item)
    }

    let rect2 = Rectangle { length: 50, width: 30 };
    println!("rect2 is {}", rect2.area());

    println!("rect2 saure {}", Rectangle::square(3).area())
}
