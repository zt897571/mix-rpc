// Package xrpc -----------------------------
// @file      : msg.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/1 19:44
// -------------------------------------------
package xrpc

import (
	"github.com/gogo/protobuf/proto"
	"golang/iface"
)

type processMsg struct {
	callback func()
}

type processReplyMsg struct {
	msg proto.Message
	err error
}

var _ iRpcReplyMsg = (*processReplyMsg)(nil)

func (r *processReplyMsg) getRpcResult() (proto.Message, error) {
	return r.msg, r.err
}

func newRawProcessResponse(msg proto.Message, err error) *processReplyMsg {
	return &processReplyMsg{
		msg: msg,
		err: err,
	}
}

type processReqMsg struct {
	from   iface.IPid
	target iface.IPid
	msg    proto.Message
	bCall  bool
}

var _ iProcessReqMsg = (*processReqMsg)(nil)

func newProcessReqMsg(from iface.IPid, target iface.IPid, msg proto.Message, isCall bool) *processReqMsg {
	return &processReqMsg{
		from:   from,
		target: target,
		msg:    proto.Clone(msg),
		bCall:  isCall,
	}
}

var _ iProcessReqMsg = (*processReqMsg)(nil)

func (p *processReqMsg) getSeq() uint32 {
	return 0
}

func (p *processReqMsg) getFrom() iface.IPid {
	return p.from
}

func (p *processReqMsg) getTarget() iface.IPid {
	return p.target
}

func (p *processReqMsg) isCall() bool {
	return p.bCall
}

func (p *processReqMsg) getPbMsg() proto.Message {
	return p.msg
}

func (p *processReqMsg) preDecode() error {
	return nil
}

func (p *processReqMsg) decode() error {
	return nil
}

type rawProcessReqReplyer struct {
	channel2 *timeoutChannel[iRpcReplyMsg]
}

var _ iProcessReqReplyer = (*rawProcessReqReplyer)(nil)

func newRawProcessReplyer() *rawProcessReqReplyer {
	return &rawProcessReqReplyer{
		channel2: newTimeoutChannel[iRpcReplyMsg](1),
	}
}

func (t *rawProcessReqReplyer) getReplyChannel() chan iRpcReplyMsg {
	return t.channel2.getChannel()
}

func (t *rawProcessReqReplyer) replyReq(_ uint32, msg iRpcReplyMsg) error {
	return t.channel2.write(msg)
}

func (t *rawProcessReqReplyer) getChannle() chan iRpcReplyMsg {
	return t.channel2.getChannel()
}
