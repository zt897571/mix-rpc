// Package rpc -----------------------------
// @file      : node_mgr.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/10/17 10:28
// -------------------------------------------
package rpc

import (
	"golang/common/iface"
	"sync"
)

var gMgr *Mgr

func init() {
	gMgr = &Mgr{}
}

type Mgr struct {
	nodeMap  sync.Map
	proxyMap sync.Map
}

func (m *Mgr) RegisterNode(node iface.INode) {
	m.nodeMap.Store(node.GetNodeName(), node)
}

func (m *Mgr) RemoveNode(node iface.INode) {
	m.nodeMap.Delete(node.GetNodeName())
}

func (m *Mgr) GetAllNode() []iface.INode {
	var allNode []iface.INode
	m.nodeMap.Range(func(key, value any) bool {
		allNode = append(allNode, value.(iface.INode))
		return true
	})
	return allNode
}

func (r *Mgr) GetProxy(nodename string) iface.IRpcProxy {
	if k, ok := r.proxyMap.Load(nodename); ok {
		return k.(iface.IRpcProxy)
	}
	return nil
}

func (r *Mgr) RegisterProxy(proxy iface.IRpcProxy) {
	r.proxyMap.Store(proxy.GetNodeName(), proxy)
}

func (r *Mgr) RemoveProxy(proxy iface.IRpcProxy) {
	r.proxyMap.Delete(proxy.GetNodeName())
}

func (r *Mgr) GetAllProxy() []iface.IRpcProxy {
	var allProxy []iface.IRpcProxy
	r.proxyMap.Range(func(key, value any) bool {
		allProxy = append(allProxy, value.(iface.IRpcProxy))
		return true
	})
	return allProxy
}
