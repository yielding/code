fn ten_times<F>(f: F) where F: Fn(i32) {
  for index in 0..10 {
    f(index);
  }
}

fn main() {
  ten_times(|k| println!("hello, {}", k));
}
