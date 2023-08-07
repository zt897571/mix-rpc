// Package process -----------------------------
// @file      : process_mgr.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/7/29 17:28
// -------------------------------------------

package process

import (
	"errors"
	"golang/common/iface"
	"sync"
)

var mgr *ProcessMgr
var once sync.Once

func GetProcessMgr() *ProcessMgr {
	once.Do(func() {
		mgr = newProcessMgr()
	})
	return mgr
}

type ProcessMgr struct {
	processMap sync.Map
	nameMap    sync.Map
	id         int
}

func newProcessMgr() *ProcessMgr {
	return &ProcessMgr{}
}

func (p *ProcessMgr) CreateProcess(handler iface.IProcessMsgHandler) (*Pid, error) {
	pid := NewPid(p.nextId())
	process := newProcess(pid, "", handler)
	err := p.RegisterProcess(process)
	if err != nil {
		return nil, err
	}
	go process.Run()
	return pid, nil
}

// todo: zhangtuo 重写pid实现，挂钩nodeid, 参考erlang
func (p *ProcessMgr) nextId() int {
	p.id += 1
	return p.id
}

func (p *ProcessMgr) RegisterProcess(process *Process) error {
	if process.globalName != "" {
		if p.GetPidByName(process.GetName()) != nil {
			return errors.New("process Name repeated")
		}
		p.nameMap.Store(process.globalName, process.pid)
	}
	p.processMap.Store(process.pid, process)
	return nil
}

func (p *ProcessMgr) GetPidByName(name string) *Pid {
	if v, isOk := p.nameMap.Load(name); isOk {
		return v.(*Pid)
	}
	return nil
}

func (p *ProcessMgr) GetProcess(pid *Pid) *Process {
	if v, isOk := p.processMap.Load(pid); isOk {
		return v.(*Process)
	}
	return nil
}

func (p *ProcessMgr) RemoveProcess(pid *Pid) {
	process := p.GetProcess(pid)
	if process != nil {
		p.processMap.Delete(pid)
		if process.globalName != "" {
			p.nameMap.Delete(process.globalName)
		}
	}
}
