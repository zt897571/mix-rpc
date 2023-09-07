// Package echo_server -----------------------------
// @file      : echo_service.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/18 19:18
// -------------------------------------------
package echo_server

import (
	"golang/common/log"
	iface2 "golang/common/xnet/iface"
)

type EchoService struct {
	conn iface2.IConnection
}

var _ iface2.INetMsgHandler = (*EchoService)(nil)

func (t *EchoService) SendMsg(bytes []byte) error {
	return t.conn.Send(bytes)
}

func (t *EchoService) OnReceiveMsg(msg []byte) {
	_ = t.conn.Send(msg)
}

func (t *EchoService) OnDisconnected() {
	log.Infof("Server disconnect = %s", t.conn.GetLocalAddress())
}

func (t *EchoService) GetConnection() iface2.IConnection {
	return t.conn
}

func (t *EchoService) SetConnection(info iface2.IConnection) {
	t.conn = info
}
