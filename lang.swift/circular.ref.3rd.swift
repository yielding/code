#!/usr/bin/env swift

class Country {
  let name: String
  var capitalCity: City!
  init(name: String, capitalName: String) {
    self.name = name 
    self.capitalCity = City(name: capitalName, country: self)
  }
}

class City {
  let name: String
  unowned let country: Country
  init(name: String, country: Country) {
    self.name = name
    self.country = country
  }
}

var country = Country(name: "Canada", capitalName: "Ottawa")
