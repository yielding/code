#!/usr/bin/env swift

func sigma1(n: Int) -> Int {
  var sum = 0
  for i in 1...n { sum += i }

  return sum
}

func sigma2(n: Int) -> Int {
  if n == 1 { return 1 }
  return n + sigma2(n: n-1)
}

func sigma3(list: [Int]) -> Int {
  if list.isEmpty { return 0; }
  return list.first! + sigma3(list: Array(list.dropFirst()))
}

func sigma4(_ S: Int, L: [Int]) -> Int {
  if L.isEmpty { return S }
  return sigma4(S + L.first!, L: Array(L.dropFirst()))
}

func fold1<S,L>(_ s: S, _ l: Array<L>, _ op: (S, L) -> S) -> S {
  if l.isEmpty { return s }
  return fold1(op(s, l.first!), Array(l.dropFirst()), op)
}

extension Array {
  func empty() -> Bool { 
    return self.count == 0 
  }

  var head: Element? {
    get { return self.first }
  }

  var tail: Array<Element>? {
    get {
      if self.empty() { return nil }
      return Array(self.dropFirst())
    }
  }
}

func fold2<S, L>(_ s: S, _ l: Array<L>, _ op: (S, L) -> S) -> S {
  if l.empty() { return s }
  return fold2(op(s, l.head!), l.tail!, op)
}

func sum(_ L: [Int]) -> Int {
  return fold2(0, L, (+))
}

func minimum(_ L: [Int]) -> Int {
  return fold2(L.first!, L, min)
}

func allTrue(_ L: [Bool]) -> Bool {
  return fold2(true, L, { $0 && $1 })
}

print(sigma1(n: 10))
print(sigma2(n: 10))
print(sigma3(list: Array(1...10)))
print(sigma4(0, L: Array(1...10)))
print(fold1(0, Array(1...10), (+)))
print(fold2(0, Array(1...10), (+)))
print(sum(Array(1...10)))
print(minimum(Array(1...10)))
print(allTrue([true, true, true]))
print(allTrue([true, false, true]))

