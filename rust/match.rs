fn main() {
  let program = "+ + * - /";
  let mut accumulator = 0;
  for token in program.chars() {
    match token {
      '+' => accumulator += 1,
      '-' => accumulator -= 1,
      '*' => accumulator *= 2,
      '/' => accumulator /= 2,
      _   => { }
    }
  }

  println!("The program \"{}\" calculates the value {}",
              program, accumulator);
}

