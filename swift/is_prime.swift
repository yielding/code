#!/usr/bin/env swift

import Foundation

class Test {
    var primes: [Int] = []

    init() {
    }

    func factorial(n: Int) -> Int {
        return n <= 1 ? 1 : n * factorial(n-1)
    }

    func append(n: Int) {
        primes.append(n)
    }

    func isPrime(n: Int) -> Bool {
        if n < 2 { return false }

        var limit = Int(sqrt(Float(n)))
            if limit < 2 { return true }

        for i in 2...limit {
            if n % i == 0 { return false }
        }

        return true
    }

    func Print() {
        print(primes)
    }
}

func factorial(n: Int) -> Int {
    if n <= 1 { return 1 }
    return n * factorial(n-1)
}

var t = Test()
for i in 2...100 {
    if t.isPrime(i) { t.append(i) }
}

t.Print()

