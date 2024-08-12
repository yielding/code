#[derive(Debug, PartialEq, Copy, Clone)]
enum ShirtColor {
    Red,
    Blue,
}

struct Inventory {
    shirts: Vec<ShirtColor>,
}

impl Inventory {
    fn giveway(&self, user_preference: Option<ShirtColor>) -> ShirtColor {
        user_preference.unwrap_or_else(|| self.most_stocked())
    }

    fn most_stocked(&self) -> ShirtColor {
        let mut red_no = 0;
        let mut blue_no = 0;

        for color in &self.shirts {
            match color {
                ShirtColor::Red  => red_no  += 1,
                ShirtColor::Blue => blue_no += 1,
            }
        }

        if red_no > blue_no {
            ShirtColor::Red
        } else {
            ShirtColor::Blue
        }
    }
}

fn main() {
    let store = Inventory {
        shirts: vec![ShirtColor::Blue, ShirtColor::Red, ShirtColor::Blue],
    };


    let user_pref1 = Some(ShirtColor::Red);
    let giveway1   = store.giveway(user_pref1);
    println!("The user with preferene {:?} gets {:?}",
        user_pref1, giveway1);

    let user_pref2 = None;
    let giveway2   = store.giveway(user_pref2);
    println!("The user with preferene {:?} gets {:?}",
        user_pref2, giveway2);
}
