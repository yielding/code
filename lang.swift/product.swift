#!/usr/bin/env swift

class Product {
    let name: String!
    init?(name: String) {
        self.name = name
        if name.isEmpty { return nil }
    }
}

if let p = Product(name: "") { print("not empty") } 

class CartItem: Product {
    let quantity: Int!
    init?(name: String, quantity: Int) {
        self.quantity = quantity
        super.init(name: name)
        if quantity < 1 { return nil }
    }
}

class Document {
    var name: String?

    init() {
    }

    init? (name: String) {
        self.name = name
        if name.isEmpty { return nil }
    }
}

class AutomaticallyNamedDocument: Document {
    override init() {
        super.init()
        self.name = "[Untitled]"
    }

    override init (name: String) {
        super.init()
        self.name = "[Untitled]"
        if name.isEmpty {
            self.name = "[Untitled]"
        } else {
            self.name = name 
        }
    }
}
