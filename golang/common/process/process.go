// Package process -----------------------------
// @file      : process.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/7/29 17:24
// -------------------------------------------
package process

import (
	"github.com/gogo/protobuf/proto"
	"golang/common/error_code"
	"golang/common/iface"
	"golang/common/log"
	"golang/common/rpc"
	xgame "golang/proto"
	"sync/atomic"
	"time"
)

type Status int32

const (
	Init Status = iota
	Running
	Closing
	Closed
)

type Process struct {
	pid       iface.IPid
	actor     iface.IActor
	mailbox   chan *processMsg
	stopChan  chan struct{}
	status    Status
	replyChan chan iface.IRpcReplyMsg
}

var _ iface.IProcess = (*Process)(nil)
var _ iface.IRpcReplyer = (*Process)(nil)

func newProcess(pid iface.IPid, actor iface.IActor) *Process {
	p := &Process{
		pid:       pid,
		actor:     actor,
		mailbox:   make(chan *processMsg, 10000),
		stopChan:  make(chan struct{}, 1),
		status:    Init,
		replyChan: make(chan iface.IRpcReplyMsg, 1),
	}
	actor.SetProcess(p)
	return p
}

func (p *Process) Run() {
	defer p.OnStop()
	p.actor.OnStart()
	p.loop()
}

func (p *Process) loop() {
	p.status = Running
	for {
		select {
		case Msg := <-p.mailbox:
			Msg.callback()
		case _ = <-p.stopChan:
			return
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

func (p *Process) StopAndWait() error {
	if p.GetStatus() != Running {
		return error_code.ProcessNotRunning
	}
	waitChan := make(chan struct{})
	err := p.asyncRun(func() {
		p.stopChan <- struct{}{}
		waitChan <- struct{}{}
	})
	if err != nil {
		return err
	}
	select {
	case <-waitChan:
		return nil
	case <-time.After(time.Second * 5):
		return error_code.TimeOutError
	}
}

func (p *Process) asyncRun(cb func()) error {
	if p.GetStatus() != Running {
		return error_code.ProcessNotRunning
	}
	select {
	case p.mailbox <- &processMsg{callback: cb}:
		return nil
	default:
		return error_code.ChannelIsFull
	}
}

func (p *Process) OnStop() {
	p.status = Closing
	GetProcessMgr().RemoveProcess(p.pid)
	p.actor.OnStop()
	p.status = Closed
}

// Call 只能在本携程调用
func (p *Process) Call(target iface.IPid, pbMsg proto.Message, timeout time.Duration) (proto.Message, error) {
	if target == nil || pbMsg == nil {
		return nil, error_code.ArgumentError
	}
	if target == p.pid {
		return nil, error_code.ProcessCanNotCallSelf
	}
	if target.IsLocal() {
		err := GetProcessMgr().DispatchMsg(newProcessReqMsg(p.pid, target, pbMsg, true), p)
		if err != nil {
			return nil, err
		}
		rst, err := p.waitReply(timeout)
		if err != nil {
			return nil, err
		}
		return rst.GetRpcResult()
	} else {
		return p.callRemoteProcess(target, pbMsg, timeout)
	}
}

func (p *Process) Cast(target iface.IPid, pbMsg proto.Message) error {
	if target == nil || pbMsg == nil {
		return error_code.ArgumentError
	}
	if target.IsLocal() {
		return GetProcessMgr().DispatchMsg(newProcessReqMsg(p.pid, target, pbMsg, false), nil)
	} else {
		return p.castRemoteProcess(target, pbMsg)
	}
}

func (p *Process) castRemoteProcess(target iface.IPid, pbMsg proto.Message) error {
	if pbMsg == nil || target == nil {
		return error_code.ArgumentError
	}
	proxy, err := getRpcProxy(target)
	if err != nil {
		return err
	}
	rpcParams, err := rpc.BuildRpcParams(pbMsg)
	if err != nil {
		return err
	}
	msg := &xgame.ProcessMsg{
		Source: p.pid.Encode(),
		Target: target.Encode(),
		Params: rpcParams,
	}
	return proxy.SendProcessMsg(0, false, msg)
}

func (p *Process) callRemoteProcess(target iface.IPid, pbMsg proto.Message, timeout time.Duration) (proto.Message, error) {
	if pbMsg == nil || target == nil {
		return nil, error_code.ArgumentError
	}
	proxy, err := getRpcProxy(target)
	if err != nil {
		return nil, err
	}
	rpcParams, err := rpc.BuildRpcParams(pbMsg)
	if err != nil {
		return nil, err
	}
	msg := &xgame.ProcessMsg{
		Source: p.pid.Encode(),
		Target: target.Encode(),
		Params: rpcParams,
	}
	seq := proxy.NextSeq()
	proxy.RegSeq(seq, p.replyChan)
	defer proxy.UnRegSeq(seq)
	err = proxy.SendProcessMsg(seq, true, msg)
	if err != nil {
		return nil, err
	}
	rst, err := p.waitReply(timeout)
	if err != nil {
		return nil, err
	}
	return rst.GetRpcResult()
}

func (p *Process) GetPid() iface.IPid {
	return p.pid
}

func (p *Process) onReq(msg iface.IProcessReqMsg, responser iface.IRpcReplyer) {
	if msg.IsCall() {
		var rst proto.Message
		var err error
		defer func() {
			err = responser.ReplyReq(msg.GetSeq(), newRawProcessResponse(rst, err))
			if err != nil {
				log.Errorf("reply msg error = %v", err)
				return
			}
		}()
		err = msg.Decode()
		if err != nil {
			return
		}
		// todo recover
		rst, err = p.actor.HandleCall(msg.GetFrom(), msg.GetPbMsg())
	} else {
		p.actor.HandleCast(msg.GetFrom(), msg.GetPbMsg())
	}
}

func (p *Process) waitReply(timeout time.Duration) (iface.IRpcReplyMsg, error) {
	select {
	case <-time.After(timeout):
		return nil, error_code.TimeOutError
	case rst := <-p.replyChan:
		return rst, nil
	}
}

func (p *Process) ReplyReq(_ uint32, message iface.IRpcReplyMsg) error {
	select {
	case p.replyChan <- message:
		return nil
	default:
		return error_code.ProcessReplyError
	}
}

func (p *Process) getActor() iface.IActor {
	return p.actor
}

func getRpcProxy(target iface.IPid) (iface.IRpcProxy, error) {
	if target == nil {
		return nil, error_code.ArgumentError
	}
	node := target.GetHost()
	proxyMgr := rpc.GetRpcProxyMgr()
	proxy := proxyMgr.GetProxy(node)
	if proxy == nil {
		return nil, error_code.RpcProxyNotFound
	}
	return proxy, nil
}
