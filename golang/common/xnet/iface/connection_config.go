// Package tcp -----------------------------
// @file      : connection_config.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/16 19:14
// -------------------------------------------
package iface

import "encoding/binary"

type ConnectionConfig struct {
	ByteOrder       binary.ByteOrder
	PkgHeadLen      int
	ReadChanelSize  int
	WriteChanelSize int
}

func NewDefaultConnectionConfig() *ConnectionConfig {
	return &ConnectionConfig{
		ByteOrder:       binary.BigEndian,
		PkgHeadLen:      4,
		ReadChanelSize:  1000,
		WriteChanelSize: 1000,
	}
}
