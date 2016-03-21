#!/usr/bin/env swift

enum VendingMachineError: ErrorType {
    case InvalidSelection
    case InsufficientFunds(censNeeded: Int)
    case OutOfStock
}

struct Item {
    var price: Int
    var count: Int
}

var inventory = [
    "Candy Bar" : Item(price: 125, count: 7),
    "Chips"     : Item(price: 100, count: 4),
    "Pretzels"  : Item(price: 75, count: 3)
]

var amountDeposited = 100

func vend(itemNamed name: String) throws {
    guard var item = inventory[name] else {
        throw VendingMachineError.InvalidSelection
    }

    guard item.count > 0 else {
        throw VendingMachineError.OutOfStock
    }

    if amountDeposited >= item.price {
        amountDeposited -= item.price
        --item.count
        inventory[name] = item
    } else {
        let amountNeeded = item.price - amountDeposited 
        throw
            VendingMachineError.InsufficientFunds(censNeeded: amountNeeded)
    }
}

let favoriteSnacks = [
    "Alice" : "Chips",
    "Bob" : "Licorice",
    "Eve" : "Pretzels"
]

func buyFavoriteSnack(person: String) throws {
    let snackName = favoriteSnacks[person] ?? "Candy Bar"
    try vend(itemNamed: snackName)
}

do {
    try vend(itemNamed: "Candy Bar")
} catch VendingMachineError.InvalidSelection {
    print("InvalidSelection.")
} catch VendingMachineError.OutOfStock {
    print("OutOfStock.")
} catch VendingMachineError.InsufficientFunds(let amountNeeded) {
    print("Insufficient funds. Please insert an additional \(amountNeeded) cents.")
}

