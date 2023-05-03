#!/usr/bin/env swift

class StepCounter {
  var totalSteps: Int = 0 {
    willSet(val) {
      print("about to set to \(val)")
    }

    didSet {
      if totalSteps > oldValue {
        print("Added \(totalSteps - oldValue)")
      }
    }
  }
}

let stepCounter = StepCounter()
stepCounter.totalSteps = 200
