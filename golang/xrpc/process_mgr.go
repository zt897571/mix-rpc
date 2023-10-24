// Package process -----------------------------
// @file      : process_mgr.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/7/29 17:28
// -------------------------------------------

package xrpc

import (
	"golang/error_code"
	"golang/iface"
	"sync"
)

var gProcessMgr = &processMgr{}

type processMgr struct {
	processMap sync.Map
}

func CreateProcess(actor iface.IActor) (iface.IPid, error) {
	id := newPid()
	process := newProcess(id, actor)
	gProcessMgr.registerProcess(process)
	startedChan := make(chan struct{})
	go process.run(startedChan)
	<-startedChan
	return id, nil
}

func GetProcess(pid iface.IPid) *Process {
	if v, isOk := gProcessMgr.processMap.Load(pid.String()); isOk {
		return v.(*Process)
	}
	return nil
}

func StopProcess(pid iface.IPid) {
	if process := GetProcess(pid); process != nil {
		process.Stop()
	}
}

func (p *processMgr) registerProcess(process *Process) {
	p.processMap.Store(process.pid.String(), process)
}

func (p *processMgr) removeProcess(pid iface.IPid) {
	p.processMap.Delete(pid.String())
}

func (p *processMgr) dispatchCallMsg(msg IProcessReqMsg, responser IRpcReplyer) error {
	if responser == nil || msg == nil {
		return error_code.ArgumentError
	}
	err := msg.PreDecode()
	if err != nil {
		return err
	}
	targetPid := msg.GetTarget()
	if !targetPid.IsLocal() {
		return error_code.ProcessNotFound
	}
	process := GetProcess(targetPid)
	if process == nil {
		return error_code.ProcessNotFound
	}
	return process.asyncRun(func() {
		process.handleCall(msg, responser)
	})
}

func (p *processMgr) dispatchCastMsg(msg IProcessReqMsg) error {
	if msg == nil {
		return error_code.ArgumentError
	}
	err := msg.PreDecode()
	if err != nil {
		return err
	}
	targetPid := msg.GetTarget()
	if !targetPid.IsLocal() {
		return error_code.ProcessNotFound
	}
	process := GetProcess(targetPid)
	if process == nil {
		return error_code.ProcessNotFound
	}
	return process.asyncRun(func() {
		process.onCastReq(msg)
	})
}

func GetAllPids() []iface.IPid {
	var pids []iface.IPid
	gProcessMgr.processMap.Range(func(key, value any) bool {
		iPid := value.(iface.IProcess).GetPid()
		pids = append(pids, iPid)
		return true
	})
	return pids
}
