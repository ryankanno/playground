package main

import (
    "os"
    "strconv"
)

func fib(n int) int {
	var result int
    switch {
		case n == 0:
			result = 0
		case n == 1:
			result = 1
		default:
            result = fib(n-2) + fib(n-1)
    }
	return result
}

func main() {
    n := os.Args[1]
    i, err := strconv.Atoi(n)
    if err != nil {
        os.Exit(2) 
    }
    result := fib(i)
    println("Fibonacci value is", result)
    os.Exit(0)
}
