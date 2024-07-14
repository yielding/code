use std::env;
use std::process;

use ch12_minigrep::Config;

fn main() {
  let args: Vec<String> = env::args().collect();

  let config = Config::build(&args).unwrap_or_else(|err| {
    println!("Problem parsing argumentsL {err}");
    process::exit(1);
  });

  println!("Searching for {}", config.query);
  println!("In file {}", config.file_path);

  if let Err(e) = ch12_minigrep::run(config) {
    println!("Application error: {e}");
    process::exit(1);
  }
}
