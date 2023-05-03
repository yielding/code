#!/usr/bin/env swift

var completionHandlers: [()-> Void] = []

func someFWithEscapingClosure(completionHandler: @escaping () -> Void) {
  completionHandlers.append(completionHandler)
}

func someFWithNonEscapingClosure(closure: () -> Void) {
  closure()
}

class SomeClass {
  var x = 100
  func doSomething() {
    someFWithEscapingClosure    { self.x = 100 }
    someFWithNonEscapingClosure { x      = 200 }
  }
}


let instance = SomeClass()

instance.doSomething()
print(instance.x)

completionHandlers.first?()
print(instance.x)
