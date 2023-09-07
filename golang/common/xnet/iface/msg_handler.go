// Package iface -----------------------------
// @file      : msg_handler.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/8 18:15
// -------------------------------------------
package iface

type INetMsgHandler interface {
	OnReceiveMsg(msg []byte)
	OnDisconnected()
	SetConnection(info IConnection)
	GetConnection() IConnection
}

type IConnection interface {
	Run()
	Send([]byte) error
	Close()
	BindMsgHandler(INetMsgHandler)
	GetLocalAddress() string
	GetRemoteAddress() string
}

type INetServer interface {
	Start(INewConnection) error
	Stop()
}

type INewConnection interface {
	OnNewConnection(IConnection)
}
