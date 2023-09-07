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
	"golang/common/error_code"
	"golang/common/iface"
	"golang/common/log"
	"golang/common/rpc"
	xgame "golang/proto"
	"sync"
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
	pid        iface.IPid
	msgHandler iface.IProcessMsgHandler
	mailbox    chan *processMsg
	stopChan   chan struct{}
	status     Status
}

func newProcess(pid iface.IPid, msgHanlder iface.IProcessMsgHandler) *Process {
	p := &Process{
		pid:        pid,
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

func (p *Process) asyncRun(cb func()) error {
	select {
	case p.mailbox <- &processMsg{callback: cb}:
	default:
		return error_code.ChannelIsFull
	}
	return nil
}

func (p *Process) OnStop() {
	p.status = Closing
	GetProcessMgr().RemoveProcess(p.pid)
	p.msgHandler.OnStop()
	p.status = Closed
}

func (p *Process) OnCast(pb proto.Message) error {
	return p.asyncRun(func() {
		p.msgHandler.HandleCast(pb)
	})
}

func (p *Process) OnCall(pid iface.IPid, pb proto.Message, rstChan chan [2]any) error {
	return p.asyncRun(func() {
		rst, err := p.msgHandler.HandleCall(pid, pb)
		rstChan <- [2]any{rst, err}
	})
}

func (p *Process) Call(pid iface.IPid, pbMsg proto.Message, timeout time.Duration) (proto.Message, error) {
	if pid == nil {
		return nil, error_code.ProcessNotFound
	}
	if pid == p.pid {
		return nil, error_code.ProcessCanNotCallSelf
	}
	target := GetProcessMgr().GetProcess(pid)
	if target != nil {
		// 本地节点
		resultChan := make(chan [2]any)
		err := target.OnCall(p.pid, pbMsg, resultChan)
		if err != nil {
			return nil, err
		}
		select {
		case <-time.After(timeout):
			return nil, error_code.TimeOutError
		case rstTuple := <-resultChan:
			return rstTuple[0].(proto.Message), rstTuple[1].(error)
		}
	} else {
		//远程节点
		return p.callRemoteProcess(pid, pbMsg, timeout)
	}
}

func (p *Process) Cast(pid iface.IPid, pbMsg proto.Message) error {
	if pid == nil {
		return error_code.ProcessNotFound
	}
	target := GetProcessMgr().GetProcess(pid)
	if target != nil {
		// 本地节点
		return target.OnCast(pbMsg)
	} else {
		return p.castRemoteProcess(pid, pbMsg)
	}
}

func (p *Process) castRemoteProcess(pid iface.IPid, pbMsg proto.Message) error {
	node := pid.GetHost()
	proxyMgr := rpc.GetRpcProxyMgr()
	proxy := proxyMgr.GetProxy(node)
	if proxy == nil {
		return error_code.RpcProxyNotFound
	}
	reqMsg := &xgame.ReqMessage{
		Source: p.pid.Encode(),
		Target: pid.Encode(),
	}
	msg, err := rpc.BuildReqMsg(reqMsg, pbMsg, rpc.BuildFlag([]rpc.FlagType{rpc.REQ_FLAG}))
	if err != nil {
		return err
	}
	return proxy.Send(msg)
}

func (p *Process) callRemoteProcess(pid iface.IPid, pbMsg proto.Message, timeout time.Duration) (proto.Message, error) {
	node := pid.GetHost()
	proxyMgr := rpc.GetRpcProxyMgr()
	proxy := proxyMgr.GetProxy(node)
	if proxy == nil {
		return nil, error_code.RpcProxyNotFound
	}
	seq := proxy.NextSeq()
	reqMsg := &xgame.ReqMessage{
		Seq:    seq,
		Source: p.pid.Encode(),
		Target: pid.Encode(),
	}
	msg, err := rpc.BuildReqMsg(reqMsg, pbMsg, rpc.BuildFlag([]rpc.FlagType{rpc.CALL_FLAG, rpc.REQ_FLAG}))
	if err != nil {
		return nil, err
	}
	rstChan := make(chan *xgame.ReplyMessage)
	proxy.RegSeq(seq, rstChan)
	defer proxy.UnRegSeq(seq)
	err = proxy.Send(msg)
	if err != nil {
		return nil, err
	}
	select {
	case replyMsg := <-rstChan:
		protoMsg, err := rpc.GetProtoMsg(replyMsg.Payload, replyMsg.MsgName)
		if err != nil {
			return nil, err
		}
		if replyMsg.ErrCode != "" {
			err = errors.New(replyMsg.ErrCode)
		}
		return protoMsg, err
	case <-time.After(timeout):
		return nil, error_code.TimeOutError
	}
}

func (p *Process) GetPid() iface.IPid {
	return p.pid
}

func (p *Process) onRemoteReq(flag uint32, req *xgame.ReqMessage, proxy iface.IRpcProxy) {
	msg, err := rpc.GetProtoMsg(req.Payload, req.MsgName)
	if err != nil {
		return
	}
	sourcePid, err := DecodePid(req.Source)
	if rpc.CheckFlag(flag, rpc.REQ_FLAG) {
		// actor call
		replyMsg, err := p.msgHandler.HandleCall(sourcePid, msg)
		err = proxy.ReplyReq(req, 0, replyMsg, err)
		if err != nil {
			log.Errorf("reply msg error = %v", err)
			return
		}
	} else {
		p.msgHandler.HandleCast(msg)
	}
}
