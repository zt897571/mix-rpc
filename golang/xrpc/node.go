// Package xrpc -----------------------------
// @file      : node.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/10/16 17:14
// -------------------------------------------
package xrpc

import (
	"context"
	"github.com/gogo/protobuf/proto"
	"golang/common/log"
	"golang/error_code"
	iface2 "golang/iface"
	xgame "golang/proto"
	tcp2 "golang/xnet/tcp"
	"strings"
	"sync"
	"time"
)

const nodenameSep string = "@"

var gNode *Node

type Node struct {
	name     string
	cookie   string
	server   iface2.INetServer
	sd       iface2.IServerDiscovery // 服务发现
	proxyMap sync.Map                // proxy集合
	context  context.Context
}

func (n *Node) String() string {
	return n.name
}

func (n *Node) getAddrByNode(nodename string) (string, error) {
	return n.getServerDiscovery().GetIpAddressByNode(nodename)
}

func (n *Node) GetCookie() string {
	return n.cookie
}

func (n *Node) OnNewConnection(connection iface2.IConnection) {
	log.Infof("New Conneciton")
	rpcProxy := newRpcProxy(connection, true, "")
	rpcProxy.run()
}

func (n *Node) GetNodeName() string {
	return n.name
}

func (n *Node) getServerDiscovery() iface2.IServerDiscovery {
	return n.sd
}

func (n *Node) getProxy(nodename string) *rpcProxy {
	if k, ok := n.proxyMap.Load(nodename); ok {
		return k.(*rpcProxy)
	}
	return nil
}

func (n *Node) registerProxy(proxy *rpcProxy) {
	n.proxyMap.Store(proxy.GetNodeName(), proxy)
}

func (n *Node) removeProxy(proxy *rpcProxy) {
	n.proxyMap.Delete(proxy.GetNodeName())
}

func (n *Node) GetAllNode() []string {
	var allProxy []string
	n.proxyMap.Range(func(key, _ any) bool {
		allProxy = append(allProxy, key.(string))
		return true
	})
	return allProxy
}

func GetCookie() string {
	return gNode.cookie
}

func GetNodeName() string {
	return gNode.GetNodeName()
}

func Start(name string, cookie string) error {
	if !IsValidNodeName(name) {
		return error_code.NodeNameFormatError
	}
	if gNode != nil {
		return error_code.NodeAlreadyInit
	}
	node := &Node{
		name:    name,
		cookie:  cookie,
		sd:      newTemplateServerDiscovery(),
		context: context.Background(),
	}
	ipaddress, err := node.getAddrByNode(name)
	node.server = tcp2.NewServer(ipaddress, nil)
	errChannel := make(chan error)
	go node.server.Start(node, node.context, errChannel)
	err = <-errChannel
	if err != nil {
		return err
	}
	gNode = node
	return nil
}

// Connect 连接到远程node
func Connect(nodeName string) error {
	if !IsValidNodeName(nodeName) {
		return error_code.NodeNameFormatError
	}
	proxy := gNode.getProxy(nodeName)
	if proxy != nil {
		return nil
	}
	ipAddress, err := gNode.getAddrByNode(nodeName)
	if err != nil {
		return err
	}
	connection, err := tcp2.Connect(ipAddress, nil)
	if err != nil {
		return err
	}
	rpcProxy := newRpcProxy(connection, false, nodeName)
	gNode.registerProxy(rpcProxy)
	rpcProxy.run()
	return nil
}

func IsValidNodeName(nodeName string) bool {
	return len(strings.Split(nodeName, nodenameSep)) == 2
}

func NodeCast(nodeName string, mfa *xgame.PbMfa) error {
	if !IsValidNodeName(nodeName) {
		return error_code.NodeNameFormatError
	}
	proxy := gNode.getProxy(nodeName)
	if proxy == nil {
		return error_code.NodeNotConnected
	}
	return proxy.sendMsg(0, constNodeCastFlag, &xgame.ReqMessage{NodeMsg: mfa})
}

func NodeCall(nodeName string, mfa *xgame.PbMfa, timeout time.Duration) (proto.Message, error) {
	if !IsValidNodeName(nodeName) {
		return nil, error_code.NodeNameFormatError
	}
	proxy := gNode.getProxy(nodeName)
	if proxy == nil {
		return nil, error_code.NodeNotConnected
	}
	return proxy.blockCall(constNodeCallFlag, &xgame.ReqMessage{NodeMsg: mfa}, timeout)
}

var gMsgMap = make(map[string]iface2.IProtoMsgHandler)

func RegisterNodeMsg(module string, handler iface2.IProtoMsgHandler) {
	if _, ok := gMsgMap[module]; ok {
		log.Warnf("module %v already register", module)
	}
	gMsgMap[module] = handler
}

func applyMfa(mfa *xgame.PbMfa) (proto.Message, error) {
	if _, ok := gMsgMap[mfa.Module]; !ok {
		return nil, error_code.ModuleNotFound
	}
	msg, err := getProtoMsg(mfa.Args.Payload, mfa.Args.MsgName)
	if err != nil {
		return nil, err
	}
	return gMsgMap[mfa.Module].HandleCallMsg(mfa.Function, msg)
}
