#!/usr/bin/env swift

enum TemperatureUnit {
  case Kelvin, Celsius, Fahrenheit

  init? (symbol: Character) {
    switch symbol {
      case "K": self = .Kelvin
      case "C": self = .Celsius
      case "F": self = .Fahrenheit
      default: 
      return nil
    }
  }
}

let fahrenheitUnit = TemperatureUnit(symbol: "F")
if fahrenheitUnit != nil {
  print("This is a defined temperature unit, F")
}

let unkownUnit = TemperatureUnit(symbol: "X")
if unkownUnit == nil {
  print("init fail")
}
