// Package echo_server -----------------------------
// @file      : echo_client.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/10 16:44
// -------------------------------------------
package echo_server

import (
	"golang/common/error_code"
	"golang/common/log"
	iface2 "golang/common/xnet/iface"
	"golang/common/xnet/tcp"
	"time"
)

var CLOSE_MSG = "CLOSEMSG"

type EchoClient struct {
	conn    iface2.IConnection
	msgChan chan any
}

func NewEchoClient() *EchoClient {
	return &EchoClient{
		msgChan: make(chan any, 100),
	}
}

var _ iface2.INetMsgHandler = (*EchoClient)(nil)

func (e *EchoClient) SendMsg(msg string) error {
	return e.conn.Send([]byte(msg))
}

func (e *EchoClient) WaitResponse(timeout time.Duration) (any, error) {
	select {
	case data := <-e.msgChan:
		return data, nil
	case <-time.After(timeout):
		return "", error_code.TimeOutError
	}
}

func (e *EchoClient) OnReceiveMsg(msg []byte) {
	e.msgChan <- string(msg)
}

func (e *EchoClient) OnDisconnected() {
	log.Infof("Client disconnect Local = %s", e.conn.GetLocalAddress())
	e.msgChan <- CLOSE_MSG
}

func (e *EchoClient) SetConnection(info iface2.IConnection) {
	e.conn = info
}

func (e *EchoClient) GetConnection() iface2.IConnection {
	return e.conn
}

func connect(ipaddress string, config *iface2.ConnectionConfig) (error, *EchoClient) {
	connection, err := tcp.Connect(ipaddress, config)
	if err != nil {
		return err, nil
	}
	client := NewEchoClient()
	connection.BindMsgHandler(client)
	go connection.Run()
	return nil, client
}
