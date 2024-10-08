struct Sheep { 
  naked: bool, 
  name: &'static str 
}

trait Animal {
  fn new(name: &'static str) -> Self;

  fn name(&self)  -> &'static str;
  fn noise(&self) -> &'static str;

  fn talk(&self) {
    println!("{} says {}", self.name(), self.noise());
  }
}

impl Sheep {
  fn is_naked(&self) -> bool {
    self.naked
  }

  fn shear(&mut self) {
    if self.is_naked() {
      println!("{} is already naked.. ", self.name());
    } else {
      println!("{} gets a haircut!", self.name);

      self.naked = true;
    }
  }
}

//
// Implement the `Animal` trait for `Sheep`.
//
impl Animal for Sheep {
  // `Self` is the implementor type: `Sheep`.
  fn new(name: &'static str) -> Sheep {
    Sheep { name: name, naked: false }
  }

  fn name(&self) -> &'static str {
    self.name
  }

  fn noise(&self) -> &'static str {
    if self.is_naked() {
      "baaaaah?"
    } else {
      "baaaaah!"
    }
  }
  
  // default trait methods can be overridden.
  fn talk(&self) {
    // for example, we can add some quiet contemplation.
    println!("{} pauses briefly... {}", self.name, self.noise());
  }
}

fn main() {
  // Type annotation is necessary in this case.
  let mut dolly: Sheep = Animal::new("Dolly");

  // TODO 
  // Try removing the type annotations.

  dolly.talk();
  dolly.shear();
  dolly.talk();
}
