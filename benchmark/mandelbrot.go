package main

import (
	"fmt"
	"time"
)

const BAILOUT = 16.0
const MAX_ITERATIONS = 1000

func mandelbrot(x, y float32) int {
	cr := y - 0.5
	ci := x
	var zi float32 = 0.0
	var zr float32 = 0.0
	i := 0

	for {
		i++
		temp := zr * zi
		zr2 := zr * zr
		zi2 := zi * zi
		zr = zr2 - zi2 + cr
		zi = temp + temp + ci
		if zi2+zr2 > BAILOUT {
			return i
		}

		if i > MAX_ITERATIONS {
			return 0
		}
	}
}

func main() {
	start := time.Now()

	for y := -39; y <= 39; y++ {
		fmt.Println()
		for x := -39; x <= 39; x++ {
			result := mandelbrot(float32(x)/40.0, float32(y)/40.0)
			if result == 0 {
				fmt.Print("*")
			} else {
				fmt.Print(" ")
			}
		}
	}

	elapsed := time.Since(start).Seconds()
	fmt.Printf("\nGo Elapsed: %.2f\n", elapsed)
}
