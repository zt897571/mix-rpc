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
	xgame "golang/proto"
)

type IRpcProxy interface {
	iface.INetMsgHandler
	SetConnection(info iface.IConnection)
	NextSeq() uint32
	RegSeq(seq uint32, result chan IRpcReplyMsg)
	UnRegSeq(seq uint32)
	GetRemoteHost() string
	GetLocalHost() string
	SendNodeMsg(seq uint32, isCall bool, msg *xgame.Mfa) error
	SendProcessMsg(seq uint32, isCall bool, msg *xgame.ProcessMsg) error
}

type IRpcReplyMsg interface {
	GetRpcResult() (proto.Message, error)
}

type IRpcReplyer interface {
	ReplyReq(seq uint32, message IRpcReplyMsg) error
}
