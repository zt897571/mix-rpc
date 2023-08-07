// Package echo_server -----------------------------
// @file      : test_echo_service.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/10 16:26
// -------------------------------------------
package echo_server

import (
	iface2 "golang/common/xnet/iface"
)

type EchoServer struct {
	conn iface2.IConnection
}

var _ iface2.INetMsgHandler = (*EchoServer)(nil)

func NewEchoServer() *EchoServer {
	return &EchoServer{}
}

func (t *EchoServer) SendMsg(bytes []byte) error {
	return t.conn.Send(bytes)
}

func (t *EchoServer) OnReceiveMsg(msg []byte) {
	_ = t.conn.Send(msg)
}

func (t *EchoServer) OnDisconnected() {
}

func (t *EchoServer) SetConnection(info iface2.IConnection) {
	t.conn = info
}

func (t *EchoServer) GetConnection() iface2.IConnection {
	return t.conn
}

func (t *EchoServer) OnNewConnection(connection iface2.IConnection) {
	connection.BindMsgHandler(t)
	go connection.Run()
}
