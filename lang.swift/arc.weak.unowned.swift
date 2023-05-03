#!/usr/bin/env swift

//
// Note
//   Country 객체가 완전히 만들어지지 않은 상황에서 self 를 City에 
//   넘긴다. 그러기 위해서 한시적으로 null일 수 있는 IUO property를 사용
//
class Country {
  let name: String
  var capitalCity: City!  // '!' means implicitly unwrapped optional 
  // property

  init (name: String, capitalName: String) {
    self.name = name
    self.capitalCity = City(name: capitalName, country: self)
  }
}

class City {
  let name: String
  unowned let country: Country
  init (name: String, country: Country) {
    self.name = name
    self.country = country
  }
}

var country = Country(name: "Canada", capitalName: "Ottawa")
print("\(country.name)'s capital city is called \(country.capitalCity.name)")
