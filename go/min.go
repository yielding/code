package main

import ("fmt")

func min(a ...int) int {
  m := int(^uint(0) >> 1)
  for _, i := range a {
    if i < m {
      m = i
    }
  }

  return m
}

func main() {
  fmt.Println(min(1, 23, 4))
}
