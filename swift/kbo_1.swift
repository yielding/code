#!/usr/bin/env swift

import Foundation 

class Person: NSObject {
  @objc var name: String
  @objc var friends: [Person] = []
  @objc var bestFriend: Person? = nil

  init(name: String) {
    self.name = name
  }
}

let gabrielle = Person(name: "Gabrielle")
let jim       = Person(name: "Jim")
let yielding  = Person(name: "Chang Ha Lee")

yielding.friends = [jim, gabrielle]
yielding.bestFriend =  jim

print(#keyPath(Person.name))
print(yielding.value(forKey: #keyPath(Person.name))!)
print(yielding.value(forKeyPath: #keyPath(Person.friends.name))!)
