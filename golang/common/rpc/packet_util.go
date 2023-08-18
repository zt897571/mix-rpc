// Package rpc -----------------------------
// @file      : packet_util.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/7/31 9:55
// -------------------------------------------
package rpc

import (
	"encoding/binary"
	"github.com/gogo/protobuf/proto"
	xgame "golang/proto"
	"reflect"
)

type FlagType uint32

const (
	REQ_FLAG     FlagType = 0b1
	CALL_FLAG             = 0b10
	NODEMSG_FLAG          = 0b100
)

func BuildFlag(flagList []FlagType) uint32 {
	var result uint32
	for _, f := range flagList {
		result = uint32(f) ^ result
	}
	return result
}

func CheckFlag(flag uint32, flagType FlagType) bool {
	flagTypeUint32 := uint32(flagType)
	return flag&flagTypeUint32 == flagTypeUint32
}

func BuildReqMsg(reqMsg *xgame.ReqMessage, content proto.Message, flag uint32) ([]byte, error) {
	payload, err := proto.Marshal(content)
	if err != nil {
		return nil, err
	}
	reqMsg.Payload = payload
	reqMsg.MsgName = proto.MessageName(content)
	data, err := proto.Marshal(reqMsg)
	if err != nil {
		return nil, err
	}
	flagByte := make([]byte, 4)
	binary.BigEndian.PutUint32(flagByte, flag)
	return append(flagByte, data...), nil
}

func BuildReplyMsg(replyMsg *xgame.ReplyMessage, content proto.Message, flag uint32) ([]byte, error) {
	payload, err := proto.Marshal(content)
	if err != nil {
		return nil, err
	}
	replyMsg.MsgName = proto.MessageName(content)
	replyMsg.Payload = payload
	data, err := proto.Marshal(replyMsg)
	if err != nil {
		return nil, err
	}
	flagByte := make([]byte, 4)
	binary.BigEndian.PutUint32(flagByte, flag)
	return append(flagByte, data...), nil
}

func UnpackReqMsg(msg []byte) (*xgame.ReqMessage, proto.Message, error) {
	reqMsg := &xgame.ReqMessage{}
	err := proto.Unmarshal(msg, reqMsg)
	if err != nil {
		return nil, nil, err
	}
	protoMsg, err := GetProtoMsg(reqMsg.Payload, reqMsg.MsgName)
	if err != nil {
		return nil, nil, err
	}
	return reqMsg, protoMsg, nil
}

func GetProtoMsg(msg []byte, messageName string) (proto.Message, error) {
	tp := proto.MessageType(messageName)
	protoMsg := reflect.New(tp.Elem()).Interface().(proto.Message)
	err := proto.Unmarshal(msg, protoMsg)
	if err != nil {
		return nil, err
	}
	return protoMsg, nil
}
