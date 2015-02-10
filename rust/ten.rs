fn ten_times(f: &fn(int)) {
  let mut i = 0;
  while i < 1000 {
    f(i);
    i += 1;
  }
}

fn main() {
  ten_times(|k| println(format!("hello, %d", k)));
}
