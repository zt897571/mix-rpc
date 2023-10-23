// Package xrpc -----------------------------
// @file      : process.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/7/29 17:24
// -------------------------------------------
package xrpc

import (
	"context"
	"github.com/gogo/protobuf/proto"
	"golang/common/log"
	"golang/error_code"
	"golang/iface"
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
	IRpcReplyer
	getReplyChannel() chan IRpcReplyMsg
}

type IProcessReqMsg interface {
	GetSeq() uint32
	GetFrom() iface.IPid
	GetTarget() iface.IPid
	IsCall() bool
	GetPbMsg() proto.Message
	PreDecode() error
	Decode() error
}

type Process struct {
	pid       iface.IPid
	actor     iface.IActor
	mailbox   chan *processMsg
	status    Status
	replyChan chan IRpcReplyMsg
	cancel    context.CancelFunc
	context   context.Context
}

var _ iface.IProcess = (*Process)(nil)
var _ IRpcReplyer = (*Process)(nil)
var _ IProcessReqReplyer = (*Process)(nil)

func newProcess(pid iface.IPid, actor iface.IActor) *Process {
	p := &Process{
		pid:       pid,
		actor:     actor,
		mailbox:   make(chan *processMsg, 10000),
		status:    Init,
		replyChan: make(chan IRpcReplyMsg, 1),
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
	p.context, p.cancel = context.WithCancel(context.Background())
	p.status = Running
	for {
		select {
		case Msg := <-p.mailbox:
			Msg.callback()
		case <-p.context.Done():
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
	if p.cancel != nil {
		p.cancel()
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
	ctx, cancel := context.WithTimeout(p.context, timeout)
	defer cancel()
	return innerCall(p.pid, target, pbMsg, ctx, p)
}

func (p *Process) Cast(target iface.IPid, pbMsg proto.Message) error {
	return innerCast(p.GetPid(), target, pbMsg)
}

func (p *Process) getReplyChannel() chan IRpcReplyMsg {
	return p.replyChan
}

func (p *Process) GetPid() iface.IPid {
	return p.pid
}

func (p *Process) handleCall(msg IProcessReqMsg, responser IRpcReplyer) {
	var rst proto.Message
	var err error
	if responser == nil {
		log.Errorf("process respoonse is nil")
		return
	}
	defer func() {
		if r := recover(); r != nil {
			log.Errorf("function panic %v", r)
			err = error_code.FunctionPanicError
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

func (p *Process) onCastReq(msg IProcessReqMsg) {
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

func (p *Process) ReplyReq(_ uint32, message IRpcReplyMsg) error {
	select {
	case p.replyChan <- message:
		return nil
	default:
		return error_code.ProcessReplyError
	}
}

func (p *Process) onCall(msg IProcessReqMsg, replyer IProcessReqReplyer) error {
	return p.asyncRun(func() {
		p.handleCall(msg, replyer)
	})
}

func (p *Process) GetActor() iface.IActor {
	return p.actor
}

func getRpcProxy(target iface.IPid) (*RpcProxy, error) {
	if target == nil {
		return nil, error_code.ArgumentError
	}
	nodeName := target.GetNode()
	proxy := gNode.getProxy(nodeName)
	if proxy == nil {
		return nil, error_code.RpcProxyNotFound
	}
	return proxy, nil
}

func Call(targetPid iface.IPid, msg proto.Message, timeout time.Duration) (proto.Message, error) {
	if targetPid == nil || msg == nil {
		return nil, error_code.ArgumentError
	}
	t := newRawProcessReplyer()
	ctx, cancel := context.WithTimeout(context.TODO(), timeout)
	defer cancel()
	return innerCall(nil, targetPid, msg, ctx, t)
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
		rpcParams, err := BuildRpcParams(pbMsg)
		if err != nil {
			return err
		}
		msg := &xgame.ProcessMsg{
			Target: targetPid.Encode(),
			Params: rpcParams,
		}
		return proxy.sendMsg(0, constProcessCastFlag, msg)
	}
}

func innerCall(from iface.IPid, targetPid iface.IPid, msg proto.Message, context context.Context, replyer IProcessReqReplyer) (proto.Message, error) {
	if targetPid.IsLocal() {
		err := GetProcessMgr().DispatchCallMsg(newProcessReqMsg(from, targetPid, msg, true), replyer)
		if err != nil {
			return nil, err
		}
		return waitReply(context, replyer.getReplyChannel())
	} else {
		proxy, err := getRpcProxy(targetPid)
		if err != nil {
			return nil, err
		}
		rpcParams, err := BuildRpcParams(msg)
		if err != nil {
			return nil, err
		}
		proMsg := &xgame.ProcessMsg{
			Target: targetPid.Encode(),
			Params: rpcParams,
		}
		if from != nil {
			proMsg.Source = from.Encode()
		}
		seq := proxy.NextSeq()
		proxy.RegSeq(seq, replyer.getReplyChannel())
		defer proxy.UnRegSeq(seq)
		err = proxy.sendMsg(seq, constProcessCallFlag, &xgame.ReqMessage{ProcessMsg: proMsg})
		if err != nil {
			return nil, err
		}
		return waitReply(context, replyer.getReplyChannel())
	}
}

func waitReply(ctx context.Context, waitChan chan IRpcReplyMsg) (proto.Message, error) {
	select {
	case <-ctx.Done():
		return nil, error_code.TimeOutError
	case rst := <-waitChan:
		return rst.GetRpcResult()
	}
}