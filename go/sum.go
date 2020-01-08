package main

import "fmt"

func sum(a *[3] float64) (sum float64) {
  for _, v := range *a {
    sum += v
  }

  return
}

func main() {
  arr := [...] float64 {1.0, 2., 3.}
	fmt.Println(sum(&arr))
}
