use std::f64::consts::PI;

fn angle(vector: (f64, f64)) -> f64 {
  match vector {
    (0.0, y) if y < 0.0 => 1.5 * PI,
    (0.0, _)            => 0.5 * PI,
    (x, y)              => (y / x).atan()
  }
}

fn main() {
  println!("angle: {}", angle((1.0, 0.1)));
}
