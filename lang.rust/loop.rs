fn main() {
  let mut x = 5;
  loop {
    x += x - 3;
    if x % 5 == 0 { break; }
    println(int::to_str(x));
  }
}
