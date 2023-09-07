// Package process -----------------------------
// @file      : msg.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/1 19:44
// -------------------------------------------
package process

import (
	"github.com/gogo/protobuf/proto"
	"golang/common/iface"
)

type processMsg struct {
	callback func()
}

type processReplyMsg struct {
	msg proto.Message
	err error
}

var _ iface.IRpcReplyMsg = (*processReplyMsg)(nil)

func (r *processReplyMsg) GetRpcResult() (proto.Message, error) {
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
	isCall bool
}

var _ iface.IProcessReqMsg = (*processReqMsg)(nil)

func newProcessReqMsg(from iface.IPid, target iface.IPid, msg proto.Message, isCall bool) *processReqMsg {
	return &processReqMsg{from: from, target: target, msg: msg, isCall: isCall}
}

var _ iface.IProcessReqMsg = (*processReqMsg)(nil)

func (p *processReqMsg) GetSeq() uint32 {
	return 0
}

func (p *processReqMsg) GetFrom() iface.IPid {
	return p.from
}

func (p *processReqMsg) GetTarget() iface.IPid {
	return p.target
}

func (p *processReqMsg) IsCall() bool {
	return p.isCall
}

func (p *processReqMsg) GetPbMsg() proto.Message {
	return p.msg
}

func (p *processReqMsg) PreDecode() error {
	return nil
}

func (p *processReqMsg) Decode() error {
	return nil
}
