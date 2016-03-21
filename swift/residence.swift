#!/usr/bin/env swift

class Person {
  var residence: Residence?
}

class Room {
  let name: String
  init(name: String) { self.name = name }
}

class Address {
  var buildingName: String?
  var buildingNumber: String?
  var street: String?

  func buildingIdentifier() -> String? {
    if buildingName != nil {
      return buildingName
    } else if buildingNumber != nil && street != nil {
      return "\(buildingNumber) \(street)"
    } else {
      return nil
    }
  }
}

class Residence {
  var rooms = [Room]()
  var numberOfRooms: Int {
    return rooms.count
  }

  subscript(i: Int) -> Room {
    get { return rooms[i]     }
    set { rooms[i] = newValue }
  }

  func printNumberOfRooms() {
    print("The number or rooms os \(numberOfRooms)")
  }

  var address: Address?
}

// 1.
let john = Person()
if let roomCount = john.residence?.numberOfRooms {
  print("John's residence has \(roomCount) room(s).")
} else {
  print("Unable to retrieve the number of rooms.")
}

// 2.
let someAddress = Address()
someAddress.buildingNumber = "29"
someAddress.street = "Acacia Road"

if (john.residence?.address = someAddress) != nil {
  print("It was possible to set the address.")
} else {
  print("It was not possible to set the address.")
}

// print "It was not possible to set the address." 
