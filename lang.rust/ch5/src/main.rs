
fn first_word(s: &String) -> &str {
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return  &s[..i];
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

}
