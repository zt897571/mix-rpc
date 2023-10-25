// Package main -----------------------------
// @file      : test_actor.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/9/26 19:37
// -------------------------------------------
package main

import (
	"github.com/gogo/protobuf/proto"
	"golang/common/log"
	"golang/iface"
	"time"
)

type TestActor struct {
	iface.IProcess
}

func (t *TestActor) SetProcess(process iface.IProcess) {
	t.IProcess = process
}

func (t *TestActor) OnStart() {
	log.Infof("actor start %s\n", t.GetPid())
}

func (t *TestActor) OnStop() {
	log.Infof("actor stop %s\n", t.GetPid())
}

func (t *TestActor) HandleCast(from iface.IPid, msg proto.Message) {
	log.Infof("Receive Actor Cast from = %v, msg = %v", from, msg)
}

func (t *TestActor) HandleCall(from iface.IPid, msg proto.Message) (proto.Message, error) {
	log.Infof("Receive Actor Call from = %v, msg = %v", from, msg)
	return msg, nil
}

type ProxyActor struct {
	iface.IProcess
	pid iface.IPid
}

func (p *ProxyActor) SetProcess(process iface.IProcess) {
	p.IProcess = process
}

func (p *ProxyActor) OnStart() {
}

func (p *ProxyActor) OnStop() {
}

func (p *ProxyActor) HandleCast(from iface.IPid, msg proto.Message) {
	log.Infof("recevie cast msg")
	err := p.Cast(p.pid, msg)
	if err != nil {
		log.Errorf("Cast Error")
	}
	return
}

func (p *ProxyActor) HandleCall(from iface.IPid, msg proto.Message) (proto.Message, error) {
	log.Infof("recevie call  msg")
	return p.Call(p.pid, msg, time.Second)
}
