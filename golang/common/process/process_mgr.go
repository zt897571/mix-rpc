// Package process -----------------------------
// @file      : process_mgr.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/7/29 17:28
// -------------------------------------------

package process

import (
	"golang/common/error_code"
	"golang/common/iface"
	"golang/common/pid"
	"golang/common/rpc"
	"sync"
)

func init() {
	mgr = &ProcessMgr{}
	rpc.RegisterProcessDispatcher(mgr)
}

var mgr *ProcessMgr
var _ iface.IProcessMsgDispatcher = (*ProcessMgr)(nil)

type ProcessMgr struct {
	processMap sync.Map
}

func GetProcessMgr() *ProcessMgr {
	return mgr
}

func (p *ProcessMgr) CreateProcess(actor iface.IActor) (iface.IPid, error) {
	id := pid.NewPid()
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

func (p *ProcessMgr) DispatchCallMsg(msg iface.IProcessReqMsg, responser iface.IRpcReplyer) error {
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
		process.onCallReq(msg, responser)
	})
}

func (p *ProcessMgr) DispatchCastMsg(msg iface.IProcessReqMsg) error {
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

func (p *ProcessMgr) GetAllPids() []iface.IPid {
	var pids []iface.IPid
	p.processMap.Range(func(key, value any) bool {
		iPid := value.(iface.IProcess).GetPid()
		pids = append(pids, iPid)
		return true
	})
	return pids
}
