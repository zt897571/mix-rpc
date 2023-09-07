package main

import (
	"golang/common/rpc"
)

func main() {
	server := rpc.NewRpcServer("0.0.0.0:8000")
	err := server.Start()
	if err != nil {
		return
	}
}
