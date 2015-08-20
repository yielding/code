#!/usr/bin/env swift

class Person {
    let name: String
    var apartment: Apartment?

    init(name: String) { self.name = name }
    deinit { print("\(name) is being deinited") }
}

// Note
//  한시적으로 값이 없을 수 있는 경우, 다시 말하면 Optional인 경우 
//  weak ref를 쓴다. unowned는 Optional이 허용되지 않는 경우 
//
class Apartment {
    let unit: String
    weak var tenant: Person?

    init(unit: String) { self.unit = unit }
    
    deinit { print("Apartment \(unit) is being deinited") }
}

var john: Person?
var unit4a: Apartment?

john = Person(name: "John")
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

