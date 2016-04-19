#!/usr/bin/env swift

protocol Bird : Boolean {
  var name: String { get }
  var canFly: Bool { get }
}

extension Boolean where Self: Bird {
  var boolValue: Bool { return self.canFly }
}

protocol Flyable {
  var airspeedVelocity: Double { get }
}

extension Bird where Self: Flyable {
  var canFly: Bool { return true }
}

struct FlappyBird: Bird, Flyable {
  let name: String
  let flappyAmplitude: Double
  let flappyFrequency: Double

  var airspeedVelocity: Double {
    return 3 * flappyAmplitude * flappyFrequency
  }
}

struct Penguin: Bird {
  let name: String
  let canFly = false
}

struct SwiftBird: Bird, Flyable {
  var name: String { return "Swift \(version)" }
  let version: Double

  var airspeedVelocity: Double { return 2000.0 }
}

var res = ["flog", "pants"].map       { $0.characters.count }
                           .reduce(0) { $0 + $1 }
print(res)
