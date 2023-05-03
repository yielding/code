#!/usr/bin/env swift

func anyCommonElements<T: Sequence, U: Sequence>(_ lhs: T, _ rhs: U) -> Bool
  where T.Iterator.Element: Equatable, 
        T.Iterator.Element == U.Iterator.Element 
{
  for lhsItem in lhs {
    for rhsItem in rhs {
      if lhsItem == rhsItem { return true }
    }
  }

  return false
}

print(anyCommonElements([1, 2, 3], [3]))
