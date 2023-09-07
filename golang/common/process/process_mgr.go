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
	"golang/common/rpc"
	xgame "golang/proto"
	"sync"
)

func init() {
	mgr = &ProcessMgr{}
	rpc.RegisterProcessDispatcher(mgr)
}

var mgr *ProcessMgr
var _ iface.IRpcDispatcher = (*ProcessMgr)(nil)

type ProcessMgr struct {
	processMap sync.Map
}

func GetProcessMgr() *ProcessMgr {
	return mgr
}

func (p *ProcessMgr) CreateProcess(handler iface.IProcessMsgHandler) (iface.IPid, error) {
	pid := NewPid()
	process := newProcess(pid, handler)
	p.RegisterProcess(process)

	go process.Run()
	return pid, nil
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

func (p *ProcessMgr) DispatchMsg(flag uint32, message *xgame.ReqMessage, proxy iface.IRpcProxy) error {
	pid, err := DecodePid(message.Target)
	if err != nil {
		return err
	}
	process := p.GetProcess(pid)
	if process == nil {
		return error_code.ProcessNotFound
	}
	return process.asyncRun(func() {
		process.onRemoteReq(flag, message, proxy)
	})
}
