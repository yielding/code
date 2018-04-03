#!/usr/bin/env swift

var customersInLine = ["Christ", "Alex", "Ewa", "Barry"]

var customerProviders: [() -> String] = []

func collectCustomerProviders(_ customerProvider:
  @autoclosure @escaping () -> String) {
  customerProviders.append(customerProvider)
}

for _ in 0..< customersInLine.count {
  collectCustomerProviders(customersInLine.remove(at: 0))
}

for cp in customerProviders {
  print("Now serving \(cp()) !")
}
