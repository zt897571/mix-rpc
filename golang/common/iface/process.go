// Package iface -----------------------------
// @file      : process.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/2 11:41
// -------------------------------------------
package iface

import (
	"github.com/gogo/protobuf/proto"
	"golang/common/xnet/iface"
	xgame "golang/proto"
)

type IProcess interface {
}

type IProcessMsgHandler interface {
	SetProcess(IProcess)
	OnStart()
	OnStop()
	HandleCast(pb proto.Message)
	HandleCall(pid IPid, pb proto.Message) (proto.Message, error)
}

type HashCode string
type IHashCode interface {
	GetHashCode() HashCode
}

type NodeCallCallBack func() (proto.Message, error)
type NodeCastCallBack func()

type IRpcProxy interface {
	iface.INetMsgHandler
	SetConnection(info iface.IConnection)
	NextSeq() uint32
	RegSeq(seq uint32, result chan *xgame.ReplyMessage)
	UnRegSeq(seq uint32)
	Send([]byte) error
	GetHost() string
	ReplyReq(req *xgame.ReqMessage, flag uint32, message proto.Message, err error) error
	//GetNode() NodeType
}

type INodeMsgHandler interface {
	NodeCall(proxy IRpcProxy, message proto.Message) (proto.Message, error)
	NodeCast(proxy IRpcProxy, message proto.Message) error
}

type IPid interface {
	GetHost() string
	GetId() uint32
	Encode() []byte
}

type IPidFactory interface {
	NewPid() IPid
}

type IPidDecoder interface {
	DecodePid([]byte) (IPid, error)
	Check([]byte) bool
}

type IRpcDispatcher interface {
	DispatchMsg(flag uint32, message *xgame.ReqMessage, proxy IRpcProxy) error
}

var host string

func GetHost() string {
	return host
}

func SetHost(h string) {
	host = h
}
