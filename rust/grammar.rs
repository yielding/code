use std::float;

fn if_test (item: &str) -> () {
  let price = 
    if item == "s"      { 3.50 } 
    else if item == "m" { 2.50 } 
    else                {  1.0 };

  println(fmt!("%?", price));
}

fn is_four(x: int) -> bool {
  return x == 4;
}

fn type_test() -> () {
  let x = 4.0;
  let y: uint = x as uint;
}

fn match_test(no: int) -> bool {
  match no {
    0     => println("0"),
    1|2   => println("1 or 2"),
    3..10 => println("3 to 10"),
    _     => println("sth else")
  }

  return true;
}

fn return_test(x: int) -> int {
  if x < 0 { -1}
  else if x > 0 { 1 }
  else { 0 }
}

fn tuple_test() -> () {
  let mt: (int, int, float) = (10, 20, 30.0);
}

fn main() {
  if_test("m");
  match_test(3);
}
