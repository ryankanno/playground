package main

import (
    "net/http"
    "fmt"
    "log"
)

func handler(w http.ResponseWriter, r *http.Request) { 
    fmt.Println("Inside handler")
    fmt.Fprintf(w, "Hello world")
}

func main() {
    http.HandleFunc("/", handler)
    err := http.ListenAndServe(":9999", nil)
    if err != nil {
        log.Printf("Error running hello world: %v", err)
    }
}
