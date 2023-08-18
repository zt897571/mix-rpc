// Package process -----------------------------
// @file      : process.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/7/29 17:24
// -------------------------------------------
package process

import (
	"errors"
	"github.com/gogo/protobuf/proto"
	"golang/common/iface"
	"strconv"
	"sync"
	"sync/atomic"
)

type Pid struct {
	id string
}

func NewPid(id int) *Pid {
	atoi := strconv.Itoa(id)
	return &Pid{id: atoi}
}

type Status int32

const (
	Init Status = iota
	Running
	Closing
	Closed
)

type Process struct {
	pid        *Pid
	globalName string // 唯一名称
	msgHandler iface.IProcessMsgHandler
	mailbox    chan *processMsg
	stopChan   chan struct{}
	status     Status
}

func newProcess(pid *Pid, globalName string, msgHanlder iface.IProcessMsgHandler) *Process {
	p := &Process{
		pid:        pid,
		globalName: globalName,
		msgHandler: msgHanlder,
		mailbox:    make(chan *processMsg, 1000),
		stopChan:   make(chan struct{}, 1),
		status:     Init,
	}
	msgHanlder.SetProcess(p)
	return p
}

func (p *Process) Run() {
	defer p.OnStop()
	p.msgHandler.OnStart()
	p.loop()
}

func (p *Process) loop() {
	p.status = Running
	for {
		select {
		case Msg := <-p.mailbox:
			Msg.callback()
		case _ = <-p.stopChan:
			break
		}
	}
}

func (p *Process) GetStatus() Status {
	return Status(atomic.LoadInt32((*int32)(&p.status)))
}

func (p *Process) SetStatus(st Status) {
	atomic.StoreInt32((*int32)(&p.status), int32(st))
}

func (p *Process) Stop() {
	if p.GetStatus() != Running {
		return
	}
	p.stopChan <- struct{}{}
}

func (p *Process) StopAndWait() {
	if p.GetStatus() != Running {
		return
	}
	var wt sync.WaitGroup
	wt.Add(1)
	p.asyncRun(func() {
		defer wt.Done()
		p.stopChan <- struct{}{}
	})
	wt.Wait()
	// todo: 考虑加入超时
}

func (p *Process) asyncRun(cb func()) {
	p.mailbox <- &processMsg{callback: cb}
}

func (p *Process) OnStop() {
	p.status = Closing
	GetProcessMgr().RemoveProcess(p.pid)
	p.msgHandler.OnStop()
	p.status = Closed
}

func (p *Process) OnReceiveMsg(pb proto.Message) {
	p.asyncRun(func() {
		//msg, err := p.msgHandler.OnReceiveMsg(pb)
	})
}

func (p *Process) Call(pid *Pid, pbMsg proto.Message) (proto.Message, error) {
	if pid == nil || pid == p.pid {
		return nil, errors.New("get target process error")
	}
	target := GetProcessMgr().GetProcess(pid)
	if target != nil {
		target.OnReceiveMsg(pbMsg)
	} else {
		//远程节点
	}
	return nil, nil
}

func (p *Process) GetName() string {
	return p.globalName
}

func (p *Process) GetPid() *Pid {
	return p.pid
}
