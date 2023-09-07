// Package rpc -----------------------------
// @file      : rpc_proxy_mgr.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/14 18:44
// -------------------------------------------
package rpc

import (
	"github.com/gogo/protobuf/proto"
	"golang/common/error_code"
	iface2 "golang/common/iface"
	"golang/common/utils"
	xgame "golang/proto"
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
	return &RpcProxyMgr{}
}

type RpcProxyMgr struct {
	proxyMap sync.Map
}

func (r *RpcProxyMgr) GetProxy(host string) iface2.IRpcProxy {
	if k, ok := r.proxyMap.Load(host); ok {
		return k.(iface2.IRpcProxy)
	}
	return nil
}

func (r *RpcProxyMgr) RegisterProxy(proxy iface2.IRpcProxy) {
	r.proxyMap.Store(proxy.GetRemoteHost(), proxy)
}

func (r *RpcProxyMgr) RemoveProxy(proxy iface2.IRpcProxy) {
	r.proxyMap.Delete(proxy.GetRemoteHost())
}

func (r *RpcProxyMgr) GetAllProxy() []iface2.IRpcProxy {
	var allProxy []iface2.IRpcProxy
	r.proxyMap.Range(func(key, value any) bool {
		allProxy = append(allProxy, value.(iface2.IRpcProxy))
		return true
	})
	return allProxy
}

var mhMap = make(map[string]map[string]*utils.FuncDesc)

func RegisterNodeMsg(module string, instance any) {
	fs := utils.ScanFunction(instance)
	mp := make(map[string]*utils.FuncDesc)
	for _, f := range fs {
		mp[f.Name] = f
	}
	mhMap[module] = mp
}

func applyMfa(mfa *xgame.Mfa) (proto.Message, error) {
	var funcMap map[string]*utils.FuncDesc
	var ok bool
	if funcMap, ok = mhMap[mfa.Module]; !ok {
		return nil, error_code.MfaError
	}
	if _, ok := funcMap[mfa.Function]; !ok {
		return nil, error_code.MfaError
	}
	msg, err := GetProtoMsg(mfa.Args.Payload, mfa.Args.MsgName)
	if err != nil {
		return nil, err
	}
	rst, err := funcMap[mfa.Function].SafeCall(msg)
	if err != nil {
		return nil, err
	}
	if len(rst) == 0 {
		return nil, nil
	}
	if len(rst) != 2 {
		return nil, error_code.MfaError
	}
	var result proto.Message
	if rst[0].Interface() != nil {
		result = rst[0].Interface().(proto.Message)
	}
	if rst[1].Interface() != nil {
		err = rst[1].Interface().(error)
	}
	return result, err
}
