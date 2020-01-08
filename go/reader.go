package main

import "fmt"

type Reader interface {
  Read(p []byte) (n int, err error)
}

type Writer interface {
  Write(p []byte) (n int, err error)
}

type ReadWriter interface {
  Reader
  Writer
}

func main() {
	fmt.Println()
}
