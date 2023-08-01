// Package process -----------------------------
// @file      : process.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/7/29 17:24
// -------------------------------------------
package process

type Pid struct {
	name string
}

type Process struct {
	pid        *Pid
	globalName string // 唯一名称
	msgHandler IProcessMsgHandler
}

type IProcessMsgHandler interface {
}

func NewProcess(pid *Pid, globalName string, msgHanlde IProcessMsgHandler) *Process {
	return &Process{
		msgHandler: msgHanlde,
	}
}

func (p *Process) Start() {
}

func (p *Process) Run() {
}

func (p *Process) Stop() {
}

func (p *Process) GetName() string {
	return p.globalName
}

func (p *Process) GetPid() *Pid {
	return p.pid
}
