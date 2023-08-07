// Package rpc -----------------------------
// @file      : rpc_server.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/14 14:54
// -------------------------------------------
package rpc

import (
	iface2 "golang/common/xnet/iface"
	"golang/common/xnet/tcp"
)

type RpcServer struct {
	server iface2.IServer
}

func NewRpcServer(port int) *RpcServer {
	tcpServer := tcp.NewServer(port, nil)
	svr := &RpcServer{server: tcpServer}
	return svr
}

func (r *RpcServer) Start() {
	r.server.Start(r)
}

func (r *RpcServer) OnNewConnection(connection iface2.IConnection) {
	rpcProxy := NewRpcProxy()
	connection.BindMsgHandler(rpcProxy)
	go func() {
		GetRpcProxyMgr().RegisterProxy(rpcProxy)
		defer GetRpcProxyMgr().RemoveProxy(rpcProxy)
		connection.Run()
	}()
}
