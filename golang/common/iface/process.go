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
	"time"
)

type IProcess interface {
}

type IProcessMsgHandler interface {
	SetProcess(IProcess)
	OnStart()
	OnStop()
	OnReceiveMsg(pb proto.Message) (proto.Message, error)
}

type HashCode string
type IHashCode interface {
	GetHashCode() HashCode
}

type NodeCallCallBack func() (proto.Message, error)
type NodeCastCallBack func()

type IRpcProxy interface {
	IHashCode
	iface.INetMsgHandler
	NodeCall(message proto.Message, timeout time.Duration) (proto.Message, error)
	NodeCast(message proto.Message) error
}

type INodeMsgHandler interface {
	NodeCall(proxy IRpcProxy, message proto.Message) (proto.Message, error)
	NodeCast(proxy IRpcProxy, message proto.Message) error
}
