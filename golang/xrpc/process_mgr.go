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

var gProcessMgr = &ProcessMgr{}

type ProcessMgr struct {
	processMap sync.Map
}

func GetProcessMgr() *ProcessMgr {
	return gProcessMgr
}

func (p *ProcessMgr) CreateProcess(actor iface.IActor) (iface.IPid, error) {
	id := newPid()
	process := newProcess(id, actor)
	GetProcessMgr().RegisterProcess(process)
	go process.Run()
	return id, nil
}

func (p *ProcessMgr) RegisterProcess(process *Process) {
	p.processMap.Store(process.pid.String(), process)
}

func (p *ProcessMgr) GetProcess(pid iface.IPid) *Process {
	if v, isOk := p.processMap.Load(pid.String()); isOk {
		return v.(*Process)
	}
	return nil
}

func (p *ProcessMgr) RemoveProcess(pid iface.IPid) {
	p.processMap.Delete(pid.String())
}

func (p *ProcessMgr) DispatchCallMsg(msg IProcessReqMsg, responser IRpcReplyer) error {
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
	process := p.GetProcess(targetPid)
	if process == nil {
		return error_code.ProcessNotFound
	}
	return process.asyncRun(func() {
		process.handleCall(msg, responser)
	})
}

func (p *ProcessMgr) DispatchCastMsg(msg IProcessReqMsg) error {
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
	process := p.GetProcess(targetPid)
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
