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
}

type IActor interface {
	SetProcess(IProcess)
	OnStart()
	OnStop()
	HandleCast(from IPid, msg proto.Message)
	HandleCall(from IPid, msg proto.Message) (proto.Message, error)
}

type IPid interface {
	GetHost() string
	GetId() uint32
	Encode() []byte
	IsLocal() bool
}

type IProcessResponser interface {
	ReplyReq(seq uint32, message IRpcReplyMsg) error
}

type IProcessReqMsg interface {
	GetSeq() uint32
	GetFrom() IPid
	GetTarget() IPid
	IsCall() bool
	GetPbMsg() proto.Message
	PreDecode() error
	Decode() error
}

type IProcessMsgDispatcher interface {
	DispatchMsg(reqMsg IProcessReqMsg, responser IProcessResponser) error
}
