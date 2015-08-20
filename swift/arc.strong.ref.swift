#!/usr/bin/env swift

class Person {
    let name: String
    var apartment: Apartment?

    init(name: String) { self.name = name }
    deinit { print("\(name) is being deinited") }
}


class Apartment {
    let unit: String
    weak var tenant: Person?

    init(unit: String) { self.unit = unit }
    
    deinit { print("Apartment \(unit) is being deinited") }
}

var john: Person?
var unit4a: Apartment?


john = Person(name: "John Appleseed")
unit4a = Apartment(unit: "4A")

// ! is used to unwrap and access the instances stored inside 
// the john and unit4a optional variable


// 1. john contains string ref to apartment
john!.apartment = unit4a
unit4a!.tenant  = john

// 2. disconnect string ref 
//    called destructor of john
john = nil


// 3. no more string ref to unit4a
//    destructor is called
unit4a = nil

