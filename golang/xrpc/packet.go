// Package xrpc -----------------------------
// @file      : packet.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/9/18 19:38
// -------------------------------------------
package xrpc

import (
	"encoding/binary"
	"errors"
	"github.com/gogo/protobuf/proto"
	"golang/error_code"
	"golang/iface"
	xgame "golang/proto"
)

type packet struct {
	flag    flagType
	seq     uint32
	payload []byte
}

var _ iRpcReplyMsg = (*packet)(nil)

func newPacket(binData []byte) (*packet, error) {
	if binData == nil || len(binData) < 6 {
		return nil, error_code.PacketFormatError
	}
	flagInt := binary.BigEndian.Uint16(binData[:2])
	flag := flagType(flagInt)
	seq := binary.BigEndian.Uint32(binData[2:6])
	payload := binData[6:]
	return &packet{
		flag:    flag,
		seq:     seq,
		payload: payload,
	}, nil
}

func (p *packet) isCall() bool {
	return checkFlag(p.flag, CALL_FLAG)
}

func (p *packet) isReq() bool {
	return checkFlag(p.flag, REQ_FLAG)
}

func (p *packet) isNodeMsg() bool {
	return checkFlag(p.flag, NODEMSG_FLAG)
}

func (p *packet) isVerifyMsg() bool {
	return checkFlag(p.flag, VERIFYMSG_FLAG)
}

func (p *packet) getSeq() uint32 {
	return p.seq
}

func (p *packet) asprocessReqMsg2() *remoteProcessReqMsg {
	return newprocessReqMsg2(p)
}

func (p *packet) getRpcResult() (proto.Message, error) {
	if p.isReq() {
		return nil, error_code.PacketFormatError
	}
	if p.isVerifyMsg() {
		response := &xgame.ReplyVerify{}
		err := proto.Unmarshal(p.payload, response)
		if err != nil {
			return nil, err
		}
		return response, nil
	} else {
		response := &xgame.ReplyMessage{}
		err := proto.Unmarshal(p.payload, response)
		if err != nil {
			return nil, err
		}
		if response.Error != "" {
			return nil, errors.New(response.Error)
		} else if response.MsgName != "" {
			return getProtoMsg(response.Payload, response.MsgName)
		}
		return nil, nil
	}
}

type remoteProcessReqMsg struct {
	*packet
	processMsg *xgame.ProcessMsg
	targetPid  iface.IPid
	fromPid    iface.IPid
	msg        proto.Message
}

var _ iProcessReqMsg = (*remoteProcessReqMsg)(nil)

func newprocessReqMsg2(pkt *packet) *remoteProcessReqMsg {
	return &remoteProcessReqMsg{packet: pkt}
}

func (p *remoteProcessReqMsg) preDecode() error {
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
	targetPid, err := DecodePid(req.ProcessMsg.Target)
	if err != nil {
		return err
	}
	p.processMsg = req.ProcessMsg
	p.targetPid = targetPid
	return nil
}

func (p *remoteProcessReqMsg) decode() error {
	if p.processMsg == nil || p.processMsg.Params == nil {
		return error_code.PacketFormatError
	}
	var from iface.IPid
	if p.processMsg.Source != nil {
		fromPid, err := DecodePid(p.processMsg.Source)
		if err != nil {
			return err
		}
		from = fromPid
	}
	msg, err := getProtoMsg(p.processMsg.Params.Payload, p.processMsg.Params.MsgName)
	if err != nil {
		return err
	}
	p.payload = nil
	p.processMsg = nil
	p.msg = msg
	p.fromPid = from
	return nil
}

func (p *remoteProcessReqMsg) getFrom() iface.IPid {
	return p.fromPid
}

func (p *remoteProcessReqMsg) getTarget() iface.IPid {
	return p.targetPid
}

func (p *remoteProcessReqMsg) isCall() bool {
	return p.isCall()
}

func (p *remoteProcessReqMsg) getPbMsg() proto.Message {
	return p.msg
}

func buildMsg(flag flagType, seq uint32, msg proto.Message) ([]byte, error) {
	msgBin, err := proto.Marshal(msg)
	if err != nil {
		return nil, err
	}
	binData := make([]byte, 6)
	binary.BigEndian.PutUint16(binData, uint16(flag))
	binary.BigEndian.PutUint32(binData[2:], seq)
	return append(binData, msgBin...), nil
}
