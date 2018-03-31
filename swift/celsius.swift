#!/usr/bin/env swift

struct Celsius {
  var temperatureInCelsius: Double
  init (fromFahrenheit fahrenheit: Double) {
    temperatureInCelsius = (fahrenheit - 32.0) / 1.8
  }

  init (fromKelvin kelvin: Double) {
    temperatureInCelsius = kelvin - 273.15
  }

  init (_ celsius: Double) {
    temperatureInCelsius = celsius
  }
}

let boilingPointOfWater = Celsius(fromFahrenheit: 212.0)
print(boilingPointOfWater.temperatureInCelsius)

let freezingPointOfWater = Celsius(fromKelvin: 273.15)
print(freezingPointOfWater.temperatureInCelsius)
