#!/usr/bin/env swift

class Customer {
    let name: String
    var card: CreditCard?
    init (name: String) {
        self.name = name
    }

    deinit { print("\(name) is being deinitialized") }
}

class CreditCard {
    let number: UInt64
    unowned let customer: Customer
    init (number: UInt64, customer: Customer) {
        self.number   = number
        self.customer = customer
    }

    deinit { print("Card #\(number) is being deinitialized") }
}

var john: Customer?

john = Customer(name: "John")
john!.card = CreditCard(number: 1234_5678_1234_2345, customer: john!)

john = nil
