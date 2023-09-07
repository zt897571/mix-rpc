// Package rpc -----------------------------
// @file      : rpc_proxy_mgr.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/14 18:44
// -------------------------------------------
package rpc

import (
	iface2 "golang/common/iface"
	"sync"
)

var once sync.Once
var _mgr *RpcProxyMgr

func GetRpcProxyMgr() *RpcProxyMgr {
	once.Do(func() {
		_mgr = newRpcProxyMgr()
	})
	return _mgr
}

func newRpcProxyMgr() *RpcProxyMgr {
	return &RpcProxyMgr{
		mhMap: map[string]iface2.INodeMsgHandler{},
	}
}

type RpcProxyMgr struct {
	proxyMap sync.Map
	rwlock   sync.RWMutex
	mhMap    map[string]iface2.INodeMsgHandler
}

func (r *RpcProxyMgr) GetProxy(host string) iface2.IRpcProxy {
	if k, ok := r.proxyMap.Load(host); ok {
		return k.(iface2.IRpcProxy)
	}
	return nil
}

func (r *RpcProxyMgr) RegisterProxy(proxy iface2.IRpcProxy) {
	r.proxyMap.Store(proxy.GetHost(), proxy)
}

func (r *RpcProxyMgr) RemoveProxy(proxy iface2.IRpcProxy) {
	r.proxyMap.Delete(proxy.GetHost())
}

func (r *RpcProxyMgr) RegisterNodeMsg(protoName string, handler iface2.INodeMsgHandler) {
	r.rwlock.Lock()
	defer r.rwlock.Unlock()
	r.mhMap[protoName] = handler
}

func (r *RpcProxyMgr) GetNodeMsgHandler(protoName string) iface2.INodeMsgHandler {
	r.rwlock.RLock()
	defer r.rwlock.RUnlock()
	if h, ok := r.mhMap[protoName]; ok {
		return h
	}
	return nil
}
