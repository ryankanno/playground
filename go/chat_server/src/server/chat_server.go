package main

import (
    "client"
    "container/list"
    "fmt"
	"log"
	"net"
	"os"
)

const listenAddress = ":6969"

var clients = list.New() 

func broadcastHandler(messageChannel chan string) {
    for {
        message := <-messageChannel
        for c := clients.Front(); c != nil; c = c.Next() {
            cl := c.Value.(*client.Client)
            cl.Send(message)
        }
    }
}

func readHandler(cl *client.Client) {
    buffer := make([]byte, 1024)
    for bytesRead, result := cl.Read(buffer); result; bytesRead, result = cl.Read(buffer) {
        message := string(buffer[0:bytesRead])
        cl.Incoming <- fmt.Sprintf("%s", message)
    }
}

func addClientHandler(addClientChannel chan *client.Client) {
    for {
        client := <-addClientChannel
        clients.PushBack(client)
    }
}

func connectionHandler(conn net.Conn, messageChannel chan string, addClientChannel chan *client.Client) {
    c := &client.Client{conn, messageChannel}
    addClientChannel <-c
    go readHandler(c)
}

func main() {
	listener, err := net.Listen("tcp", listenAddress)
	if err != nil {
		log.Fatal(err)
        os.Exit(1)
	} else {
		defer listener.Close()

        addClientChannel := make(chan *client.Client) 
        go addClientHandler(addClientChannel)

        messageChannel := make(chan string) 
        go broadcastHandler(messageChannel)

		for {
			conn, err := listener.Accept()
			if err != nil {
				log.Fatal(err)
			} else {
				go connectionHandler(conn, messageChannel, addClientChannel)
			}
		}
	}
}
