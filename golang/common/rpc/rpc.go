// Package rpc -----------------------------
// @file      : rpc.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/10/18 17:17
// -------------------------------------------
package rpc

import (
	"github.com/gogo/protobuf/proto"
	"golang/common/error_code"
	"golang/common/iface"
	"golang/common/xnet/tcp"
	xgame "golang/proto"
	"time"
)

var gNode *Node

func GetNode() iface.INode {
	return gNode
}

func GetCookie() string {
	return gNode.cookie
}

func GetNodeName() string {
	return gNode.GetNodeName()
}

func Start(name string, cookie string) error {
	if !iface.IsValidNodeName(name) {
		return error_code.NodeNameFormatError
	}
	if gNode != nil {
		return error_code.NodeAlreadyInit
	}
	node := &Node{name: name, cookie: cookie}
	node.server = tcp.NewServer(node.getAddr(), nil)
	errChannel := make(chan error)
	go node.server.Start(node, errChannel)
	err := <-errChannel
	if err != nil {
		return err
	}
	gNode = node
	return nil
}

func Connect(IpAddress string) (iface.IRpcProxy, error) {
	connection, err := tcp.Connect(IpAddress, nil)
	if err != nil {
		return nil, err
	}
	rpcProxy := NewRpcProxy()
	connection.BindMsgHandler(rpcProxy)
	go func() {
		mgr := GetRpcProxyMgr()
		mgr.RegisterProxy(rpcProxy)
		defer mgr.RemoveProxy(rpcProxy)
		connection.Run()
	}()
	return rpcProxy, nil
}

func NodeCast(nodeName string, mfa *xgame.PbMfa) error {
	proxy, err := getProxyByNode(nodeName)
	if err != nil {
		return err
	}
	return proxy.SendNodeMsg(0, false, mfa)
}

func NodeCall(nodeName string, mfa *xgame.PbMfa, timeout time.Duration) (proto.Message, error) {
	proxy, err := getProxyByNode(nodeName)
	if err != nil {
		return nil, err
	}
	seq := proxy.NextSeq()
	replyChan := make(chan iface.IRpcReplyMsg, 1)
	proxy.RegSeq(seq, replyChan)
	defer proxy.UnRegSeq(seq)
	err = proxy.SendNodeMsg(seq, true, mfa)
	if err != nil {
		return nil, err
	}
	select {
	case replyMsg := <-replyChan:
		return replyMsg.GetRpcResult()
	case <-time.After(timeout):
		return nil, error_code.TimeOutError
	}
}

func getProxyByNode(nodeName string) (iface.IRpcProxy, error) {
	if !iface.IsValidNodeName(nodeName) {
		return nil, error_code.NodeNameFormatError
	}
	proxy := gMgr.GetProxy(nodeName)
	if proxy != nil {
		return nil, error_code.RpcNodeNotConnected
	}
	return proxy, nil
}
