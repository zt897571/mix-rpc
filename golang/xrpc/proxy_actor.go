// Package xrpc -----------------------------
// @file      : proxy_actor.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/11/3 19:07
// -------------------------------------------
package xrpc

import (
	"github.com/gogo/protobuf/proto"
	"golang/common/log"
	"golang/iface"
	"time"
)

type ProxyActor struct {
	iface.IProcess
	pid iface.IPid
}

func NewProxyActor(pid iface.IPid) *ProxyActor {
	return &ProxyActor{pid: pid}
}

func (p *ProxyActor) SetProcess(process iface.IProcess) {
	p.IProcess = process
}

func (p *ProxyActor) OnStart() {
}

func (p *ProxyActor) OnStop() {
}

func (p *ProxyActor) HandleCast(from iface.IPid, msg proto.Message) {
	err := p.Cast(p.pid, msg)
	if err != nil {
		log.Errorf("Cast Error")
	}
	return
}

func (p *ProxyActor) HandleCall(from iface.IPid, msg proto.Message) (proto.Message, error) {
	return p.Call(p.pid, msg, time.Second)
}
