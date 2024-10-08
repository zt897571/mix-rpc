// Package iface -----------------------------
// @file      : process.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/2 11:41
// -------------------------------------------
package iface

import (
	"github.com/gogo/protobuf/proto"
	"time"
)

type IProcess interface {
	Call(pid IPid, message proto.Message, duration time.Duration) (proto.Message, error)
	Cast(pid IPid, pbMsg proto.Message) error
	GetPid() IPid
	GetActor() IActor
}

type IActor interface {
	SetProcess(IProcess)
	OnStart()
	OnStop()
	HandleCast(from IPid, msg proto.Message)
	HandleCall(from IPid, msg proto.Message) (proto.Message, error)
}

type IPid interface {
	GetNode() string
	Encode() []byte
	IsLocal() bool
	String() string
}

// IServerDiscovery 服务发现接口
type IServerDiscovery interface {
	GetIpAddressByNode(string) (string, error)
}

// IProtoMsgHandler proto消息处理接口
type IProtoMsgHandler interface {
	HandleCallMsg(reqName string, msg proto.Message) (proto.Message, error)
	HandleCastMsg(reqName string, msg proto.Message) error
}
