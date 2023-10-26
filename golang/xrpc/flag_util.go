// Package xrpc -----------------------------
// @file      : packet_util.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/7/31 9:55
// -------------------------------------------
package xrpc

import (
	"github.com/gogo/protobuf/proto"
	xgame "golang/proto"
	"reflect"
)

type flagType uint16

const (
	REQ_FLAG       flagType = 0b1
	CALL_FLAG               = 0b10
	NODEMSG_FLAG            = 0b100
	VERIFYMSG_FLAG          = 0b1000
)
const (
	// node flag
	constNodeCallFlag = NODEMSG_FLAG ^ REQ_FLAG ^ CALL_FLAG
	constNodeCastFlag = NODEMSG_FLAG ^ REQ_FLAG

	//verify msg flag
	constVerifyReqFlag   = VERIFYMSG_FLAG ^ REQ_FLAG ^ CALL_FLAG
	constVerifyReplyFlag = VERIFYMSG_FLAG ^ CALL_FLAG

	// process msg flag
	constProcessCallFlag = CALL_FLAG ^ REQ_FLAG
	constProcessCastFlag = REQ_FLAG
)

func buildFlag(flagList []flagType) flagType {
	var flag flagType
	for _, f := range flagList {
		flag = f ^ flag
	}
	return flag
}

func checkFlag(flag flagType, flagType flagType) bool {
	return flag&flagType == flagType
}

func getProtoMsg(msg []byte, messageName string) (proto.Message, error) {
	tp := proto.MessageType(messageName)
	protoMsg := reflect.New(tp.Elem()).Interface().(proto.Message)
	err := proto.Unmarshal(msg, protoMsg)
	if err != nil {
		return nil, err
	}
	return protoMsg, nil
}

func buildReplyMsg(msg proto.Message, err error) *xgame.ReplyMessage {
	rst := &xgame.ReplyMessage{}
	if err != nil {
		rst.Error = err.Error()
	} else if msg != nil {
		payload, err := proto.Marshal(msg)
		if err != nil {
			rst.Error = err.Error()
			return rst
		}
		rst.MsgName = proto.MessageName(msg)
		rst.Payload = payload
	}
	return rst
}

func buildRpcParams(pbMsg proto.Message) (*xgame.RpcParams, error) {
	payload, err := proto.Marshal(pbMsg)
	if err != nil {
		return nil, err
	}
	return &xgame.RpcParams{
		MsgName: proto.MessageName(pbMsg),
		Payload: payload,
	}, nil
}
