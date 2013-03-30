package main

import (
	"os"
	"log"
	"net"
)

const listenAddress = ":6969"

func HandleConnection(conn net.Conn) {
    defer conn.Close()
	buffer := make([]byte, 4096)
	for {
		n, err := conn.Read(buffer)
		if err != nil || n == 0 {
			break
		}
		_, err = conn.Write(buffer[0:n])
		if err != nil {
			break
		}
	}
}

func main() {
	listener, err := net.Listen("tcp", listenAddress)
	if err != nil {
		log.Fatal(err)
        os.Exit(1)
	} else {
		defer listener.Close()
		for {
			conn, err := listener.Accept()
			if err != nil {
				log.Fatal(err)
			} else {
				go HandleConnection(conn)
			}
		}
	}
}
