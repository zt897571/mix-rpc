package main

import (
	"golang/common/rpc"
)

func main() {
	server := rpc.NewRpcServer(8000)
	server.Start()
}
