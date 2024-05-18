use std::f64;

fn if_test (item: &str) -> () {
  let price = 
    if item == "s"      { 3.50 } 
    else if item == "m" { 2.50 } 
    else                {  1.0 };

  println!("{}", price)
}

fn is_four(x: i32) -> bool {
  return x == 4;
}

fn match_test(no: i32) -> bool {
  match no {
    0     => println!("0"),
    1|2   => println!("1 or 2"),
    // 3..10 => println!("3 to 10"),
    _     => println!("sth else")
  }

  return true;
}

fn return_test(x: i32) -> i32 {
  if x < 0 { -1}
  else if x > 0 { 1 }
  else { 0 }
}

fn tuple_test() -> () {
  let _mt: (i32, i32, f64) = (10, 20, 30.0);
}

fn main() {
  if_test("m");
  is_four(3);
  return_test(10);
  tuple_test();
  match_test(3);
}
