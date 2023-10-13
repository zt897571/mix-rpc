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

type IProcessReqReplyer interface {
	iface.IRpcReplyer
	getReplyChannel() chan iface.IRpcReplyMsg
}

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
var _ IProcessReqReplyer = (*Process)(nil)

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
	if p.status != Init {
		return
	}
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
	return innerCall(p.pid, target, pbMsg, timeout, p)
}

func (p *Process) Cast(target iface.IPid, pbMsg proto.Message) error {
	if target == nil || pbMsg == nil {
		return error_code.ArgumentError
	}
	return innerCast(p.GetPid(), target, pbMsg)
}

func (p *Process) getReplyChannel() chan iface.IRpcReplyMsg {
	return p.replyChan
}

func (p *Process) GetPid() iface.IPid {
	return p.pid
}

func (p *Process) onCallReq(msg iface.IProcessReqMsg, responser iface.IRpcReplyer) {
	var rst proto.Message
	var err error
	if responser == nil {
		log.Errorf("process respoonse is nil")
		return
	}
	defer func() {
		if r := recover(); r != nil {
			log.Errorf("function panic %v", r)
			err = error_code.Panic
		}
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
	rst, err = p.actor.HandleCall(msg.GetFrom(), msg.GetPbMsg())
}

func (p *Process) onCastReq(msg iface.IProcessReqMsg) {
	defer func() {
		if r := recover(); r != nil {
			log.Errorf("function panic = %v", r)
			return
		}
	}()
	err := msg.Decode()
	if err != nil {
		return
	}
	p.actor.HandleCast(msg.GetFrom(), msg.GetPbMsg())
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
	nodeName := target.GetNodeName()
	proxyMgr := rpc.GetRpcProxyMgr()
	proxy := proxyMgr.GetProxy(nodeName.GetHost())
	if proxy == nil {
		return nil, error_code.RpcProxyNotFound
	}
	return proxy, nil
}

type rawProcessReqReplyer struct {
	channel chan iface.IRpcReplyMsg
}

var _ IProcessReqReplyer = (*rawProcessReqReplyer)(nil)

func newRawProcessReplyer() *rawProcessReqReplyer {
	return &rawProcessReqReplyer{
		channel: make(chan iface.IRpcReplyMsg, 1),
	}
}

func (t *rawProcessReqReplyer) getReplyChannel() chan iface.IRpcReplyMsg {
	return t.channel
}

func (t *rawProcessReqReplyer) ReplyReq(_ uint32, msg iface.IRpcReplyMsg) error {
	select {
	case t.channel <- msg:
		return nil
	default:
		return error_code.ChannelInvalid
	}
}

func (t *rawProcessReqReplyer) getChannle() chan iface.IRpcReplyMsg {
	return t.channel
}

func Call(targetPid iface.IPid, msg proto.Message, timeout time.Duration) (proto.Message, error) {
	if targetPid == nil || msg == nil {
		return nil, error_code.ArgumentError
	}
	t := newRawProcessReplyer()
	return innerCall(nil, targetPid, msg, timeout, t)
}

func Cast(targetPid iface.IPid, pbMsg proto.Message) error {
	return innerCast(nil, targetPid, pbMsg)
}

func innerCast(from iface.IPid, targetPid iface.IPid, pbMsg proto.Message) error {
	if targetPid == nil || pbMsg == nil {
		return error_code.ArgumentError
	}
	if targetPid.IsLocal() {
		return GetProcessMgr().DispatchCastMsg(newProcessReqMsg(from, targetPid, pbMsg, false))
	} else {
		proxy, err := getRpcProxy(targetPid)
		if err != nil {
			return err
		}
		rpcParams, err := rpc.BuildRpcParams(pbMsg)
		if err != nil {
			return err
		}
		msg := &xgame.ProcessMsg{
			Target: targetPid.Encode(),
			Params: rpcParams,
		}
		return proxy.SendProcessMsg(0, false, msg)
	}
}

func innerCall(from iface.IPid, targetPid iface.IPid, msg proto.Message, timeout time.Duration, replyer IProcessReqReplyer) (proto.Message, error) {
	if targetPid.IsLocal() {
		err := GetProcessMgr().DispatchCallMsg(newProcessReqMsg(from, targetPid, msg, true), replyer)
		if err != nil {
			return nil, err
		}
		rst, err := waitReply(timeout, replyer.getReplyChannel())
		if err != nil {
			return nil, err
		}
		return rst.GetRpcResult()
	} else {
		return callRemote(from, targetPid, msg, timeout, replyer.getReplyChannel())
	}
}

func callRemote(fromPid iface.IPid, targetPid iface.IPid, pbMsg proto.Message, timeout time.Duration, waitChan chan iface.IRpcReplyMsg) (proto.Message, error) {
	proxy, err := getRpcProxy(targetPid)
	if err != nil {
		return nil, err
	}
	rpcParams, err := rpc.BuildRpcParams(pbMsg)
	if err != nil {
		return nil, err
	}
	proMsg := &xgame.ProcessMsg{
		Target: targetPid.Encode(),
		Params: rpcParams,
	}
	if fromPid != nil {
		proMsg.Source = fromPid.Encode()
	}
	seq := proxy.NextSeq()
	proxy.RegSeq(seq, waitChan)
	defer proxy.UnRegSeq(seq)
	err = proxy.SendProcessMsg(seq, true, proMsg)
	if err != nil {
		return nil, err
	}
	rst, err := waitReply(timeout, waitChan)
	if err != nil {
		return nil, err
	}
	return rst.GetRpcResult()
}

// todo zhangtuo 改为使用context 整合超时流程和stop流程
func waitReply(timeout time.Duration, waitChan chan iface.IRpcReplyMsg) (iface.IRpcReplyMsg, error) {
	select {
	case <-time.After(timeout):
		return nil, error_code.TimeOutError
	case rst := <-waitChan:
		return rst, nil
	}
}
