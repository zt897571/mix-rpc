// Package rpc -----------------------------
// @file      : rpc_server.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/14 14:54
// -------------------------------------------
package rpc

import (
	"golang/common/log"
	iface2 "golang/common/xnet/iface"
	"golang/common/xnet/tcp"
)

type RpcServer struct {
	server iface2.INetServer
}

func NewRpcServer(addr string) *RpcServer {
	svr := &RpcServer{server: tcp.NewServer(addr, nil)}
	return svr
}

func (r *RpcServer) Start() error {
	errChannel := make(chan error)
	go func() {
		r.server.Start(r, errChannel)
	}()
	return <-errChannel
}

func (r *RpcServer) OnNewConnection(connection iface2.IConnection) {
	rpcProxy := NewRpcProxy()
	connection.BindMsgHandler(rpcProxy)
	go func() {
		GetRpcProxyMgr().RegisterProxy(rpcProxy)
		defer GetRpcProxyMgr().RemoveProxy(rpcProxy)
		connection.Run()
		log.Infof("Connection Stop = %v", connection.GetRemoteAddress())
	}()
}
