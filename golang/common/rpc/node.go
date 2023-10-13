// Package rpc -----------------------------
// @file      : node.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/10/16 17:14
// -------------------------------------------
package rpc

import (
	"golang/common/iface"
	"golang/common/log"
	iface2 "golang/common/xnet/iface"
	"strings"
)

const nodenameSep string = "@"

type Node struct {
	name   string
	cookie string
	server iface2.INetServer
}

var _ iface.INode = (*Node)(nil)

func (n *Node) Stop() error {
	if n.server != nil {
		n.server.Stop()
	}
	return nil
}

func (n *Node) String() string {
	return n.name
}

func (n *Node) getAddr() string {
	seps := strings.Split(n.name, nodenameSep)
	return strings.Join(seps[1:], ":")
}

func (n *Node) GetCookie() string {
	return n.cookie
}

func (n *Node) OnNewConnection(connection iface2.IConnection) {
	log.Infof("New Conneciton")
	rpcProxy := NewRpcProxy()
	connection.BindMsgHandler(rpcProxy)
	go func() {
		GetRpcProxyMgr().RegisterProxy(rpcProxy)
		defer GetRpcProxyMgr().RemoveProxy(rpcProxy)
		connection.Run()
		log.Infof("Connection Stop = %v", connection.GetRemoteAddress())
	}()
}

func (n *Node) GetNodeName() string {
	return n.name
}
