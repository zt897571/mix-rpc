// Package echo_server -----------------------------
// @file      : test_echo_service.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/10 16:26
// -------------------------------------------
package echo_server

import (
	iface2 "golang/common/xnet/iface"
	"golang/common/xnet/tcp"
)

type EchoServer struct {
	server iface2.INetServer
}

func NewEchoServer(addr string) *EchoServer {
	return &EchoServer{
		server: tcp.NewServer(addr, nil),
	}
}

func (t *EchoServer) Start() {
	t.server.Start(t)
}

func (t *EchoServer) Stop() {
	t.server.Stop()
}

func (t *EchoServer) OnNewConnection(connection iface2.IConnection) {
	ts := &EchoService{}
	connection.BindMsgHandler(ts)
	go connection.Run()
}
