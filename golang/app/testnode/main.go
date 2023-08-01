package main

import (
	"encoding/binary"
	"golang/common/xnet/tcp"
)

func main() {
	connConfig := &tcp.ConnectionConfig{
		PkgHeadLen:      4,
		ByteOrder:       binary.BigEndian,
		ReadChanelSize:  1000,
		WriteChanelSize: 1000,
	}
	server := tcp.NewServer(8000, connConfig)
	server.Start()
}
