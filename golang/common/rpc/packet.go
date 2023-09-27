// Package rpc -----------------------------
// @file      : packet.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/9/18 19:38
// -------------------------------------------
package rpc

import (
	"encoding/binary"
	"errors"
	"github.com/gogo/protobuf/proto"
	"golang/common/error_code"
	"golang/common/iface"
	"golang/common/pid"
	xgame "golang/proto"
)

type packet struct {
	flag    FlagType
	seq     uint32
	payload []byte
}

var _ iface.IRpcReplyMsg = (*packet)(nil)

func newPacket(binData []byte) (*packet, error) {
	if binData == nil || len(binData) < 6 {
		return nil, error_code.PacketFormatError
	}
	flagInt := binary.BigEndian.Uint16(binData[:2])
	flag := FlagType(flagInt)
	seq := binary.BigEndian.Uint32(binData[2:6])
	payload := binData[6:]
	return &packet{
		flag:    flag,
		seq:     seq,
		payload: payload,
	}, nil
}

func (p *packet) isCall() bool {
	return CheckFlag(p.flag, CALL_FLAG)
}

func (p *packet) isReq() bool {
	return CheckFlag(p.flag, REQ_FLAG)
}

func (p *packet) isNodeMsg() bool {
	return CheckFlag(p.flag, NODEMSG_FLAG)
}

func (p *packet) GetSeq() uint32 {
	return p.seq
}

func (p *packet) asProcessReqMsg() *processReqMsg {
	return newProcessReqMsg(p)
}

func (p *packet) GetRpcResult() (proto.Message, error) {
	if p.isReq() {
		return nil, error_code.PacketFormatError
	}
	response := &xgame.ReplyMessage{}
	err := proto.Unmarshal(p.payload, response)
	if err != nil {
		return nil, err
	}
	if response.Error != "" {
		return nil, errors.New(response.Error)
	} else if response.MsgName != "" {
		return GetProtoMsg(response.Payload, response.MsgName)
	}
	return nil, nil
}

type processReqMsg struct {
	*packet
	processMsg *xgame.ProcessMsg
	targetPid  iface.IPid
	fromPid    iface.IPid
	msg        proto.Message
}

var _ iface.IProcessReqMsg = (*processReqMsg)(nil)

func newProcessReqMsg(pkt *packet) *processReqMsg {
	return &processReqMsg{packet: pkt}
}

func (p *processReqMsg) PreDecode() error {
	if !p.isReq() || p.isNodeMsg() {
		return error_code.LogicError
	}
	req := &xgame.ReqMessage{}
	err := proto.Unmarshal(p.payload, req)
	if err != nil {
		return err
	}
	if req.ProcessMsg == nil {
		return error_code.PacketFormatError
	}
	targetPid, err := pid.DecodePid(req.ProcessMsg.Target)
	if err != nil {
		return err
	}
	p.processMsg = req.ProcessMsg
	p.targetPid = targetPid
	return nil
}

func (p *processReqMsg) Decode() error {
	if p.processMsg == nil || p.processMsg.Params == nil {
		return error_code.PacketFormatError
	}
	var from iface.IPid
	if p.processMsg.Source != nil {
		fromPid, err := pid.DecodePid(p.processMsg.Source)
		if err != nil {
			return err
		}
		from = fromPid
	}
	msg, err := GetProtoMsg(p.processMsg.Params.Payload, p.processMsg.Params.MsgName)
	if err != nil {
		return err
	}
	p.payload = nil
	p.processMsg = nil
	p.msg = msg
	p.fromPid = from
	return nil
}

func (p *processReqMsg) GetFrom() iface.IPid {
	return p.fromPid
}

func (p *processReqMsg) GetTarget() iface.IPid {
	return p.targetPid
}

func (p *processReqMsg) IsCall() bool {
	return p.isCall()
}

func (p *processReqMsg) GetPbMsg() proto.Message {
	return p.msg
}

func buildMsg(flag FlagType, seq uint32, msg proto.Message) ([]byte, error) {
	msgBin, err := proto.Marshal(msg)
	if err != nil {
		return nil, err
	}
	binData := make([]byte, 6)
	binary.BigEndian.PutUint16(binData, uint16(flag))
	binary.BigEndian.PutUint32(binData[2:], seq)
	return append(binData, msgBin...), nil
}
