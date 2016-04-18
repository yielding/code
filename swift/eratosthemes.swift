#!/usr/bin/env swift

func primes(n: Int) -> [Int] {
  var numbers = [Int](2..<n)
  for i in 0..<n-2 {
    // guard let prime = numbers[i] where prime > 0 else { continue }
    let prime = numbers[i]
    if prime == 0 { continue }
    for multiple in stride(from: 2*prime - 2, to: n-2, by: prime) {
      numbers[multiple] = 0
    }
  }

  return numbers.filter { $0 > 0 }
}

func sieve(numbers: [Int]) -> [Int] {
  if numbers.isEmpty { return [] }
  let p = numbers[0]
  return [p] + sieve(numbers: numbers[1..<numbers.count].filter { $0 % p > 0 })
}

let no = 10000
print(sieve(numbers: [Int](2..<no)))
