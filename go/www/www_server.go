package main

import (
    "net/http"
    "fmt"
    "log"
    "os"
)

func handler(w http.ResponseWriter, r *http.Request) { 
    fmt.Println("Inside handler")
    fmt.Fprintf(w, "Hello world")
}

func Log(handler http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        log.Printf("%s %s %s", r.RemoteAddr, r.Method, r.URL)
        handler.ServeHTTP(w, r)
    })
}

func main() {
    log.SetOutput(os.Stdout)
    port := ":9999"
    http.HandleFunc("/", handler)
    if err := http.ListenAndServe(port, Log(http.DefaultServeMux)); err != nil { 
		log.Printf("ListenAndServe Error: %v", err) 
	}
}
