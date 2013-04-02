package client

import (
	"log"
	"net"
)

type Client struct {
    Conn net.Conn
    Incoming chan<- string
}

func (c *Client) Send(message string) {
    _, err := c.Conn.Write([]byte(message))
    if err != nil {
        log.Printf("Error sending message to client: (%s)", err)
    }
}

func (c *Client) Read(buffer []byte) (int, bool) {
    bytesRead, err := c.Conn.Read(buffer)
    if err != nil {
        log.Printf("Error reading message to client: (%s)", err)
        return 0, false
    }
    return bytesRead, true
}
