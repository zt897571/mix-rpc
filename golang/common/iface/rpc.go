// Package iface -----------------------------
// @file      : rpc.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/9/18 18:19
// -------------------------------------------
package iface

import (
	"github.com/gogo/protobuf/proto"
	"golang/common/xnet/iface"
)

type IRpcProxy interface {
	iface.INetMsgHandler
	SetConnection(info iface.IConnection)
	NextSeq() uint32
	RegSeq(seq uint32, result chan IRpcReplyMsg)
	UnRegSeq(seq uint32)
	GetRemoteHost() string
	GetLocalHost() string
	SendMsg(seq uint32, flag FlagType, message proto.Message) error
}

type FlagType uint16

type IRpcReplyMsg interface {
	GetRpcResult() (proto.Message, error)
}
