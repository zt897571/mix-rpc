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

func (p *ProcessMgr) CreateProcess(handler iface.IActor) (iface.IPid, error) {
	id := pid.NewPid()
	process := newProcess(id, handler)
	GetProcessMgr().RegisterProcess(process)
	go process.Run()
	return id, nil
}

func (p *ProcessMgr) RegisterProcess(process *Process) {
	p.processMap.Store(process.pid, process)
}

func (p *ProcessMgr) GetProcess(pid iface.IPid) *Process {
	if v, isOk := p.processMap.Load(pid); isOk {
		return v.(*Process)
	}
	return nil
}

func (p *ProcessMgr) RemoveProcess(pid iface.IPid) {
	p.processMap.Delete(pid)
}

func (p *ProcessMgr) DispatchMsg(msg iface.IProcessReqMsg, responser iface.IProcessResponser) error {
	err := msg.PreDecode()
	if err != nil {
		return err
	}
	process := p.GetProcess(msg.GetTarget())
	if process == nil {
		return error_code.ProcessNotFound
	}
	return process.asyncRun(func() {
		process.onReq(msg, responser)
	})
}

func (p *ProcessMgr) GetAllPids() []iface.IPid {
	var pids []iface.IPid
	p.processMap.Range(func(key, value any) bool {
		pids = append(pids, key.(iface.IPid))
		return true
	})
	return pids
}
