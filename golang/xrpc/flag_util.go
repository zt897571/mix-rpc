// Package xrpc -----------------------------
// @file      : packet_util.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/7/31 9:55
// -------------------------------------------
package xrpc

import (
	"fmt"
	"github.com/gogo/protobuf/proto"
	xgame "golang/proto"
	"reflect"
	"strings"
)

type FlagType uint16

const (
	REQ_FLAG       FlagType = 0b1
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

func BuildFlag(flagList []FlagType) FlagType {
	var flag FlagType
	for _, f := range flagList {
		flag = f ^ flag
	}
	return flag
}

func CheckFlag(flag FlagType, flagType FlagType) bool {
	return flag&flagType == flagType
}

func GetProtoMsg(msg []byte, messageName string) (proto.Message, error) {
	// todo: zhangtuo fix this (兼容golang message name)
	if strings.Index(messageName, ".") == -1 {
		messageName = fmt.Sprintf("xgame.%s", messageName)
	}
	tp := proto.MessageType(messageName)
	protoMsg := reflect.New(tp.Elem()).Interface().(proto.Message)
	err := proto.Unmarshal(msg, protoMsg)
	if err != nil {
		return nil, err
	}
	return protoMsg, nil
}

func BuildReplyMsg(msg proto.Message, err error) *xgame.ReplyMessage {
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

func BuildRpcParams(pbMsg proto.Message) (*xgame.RpcParams, error) {
	payload, err := proto.Marshal(pbMsg)
	if err != nil {
		return nil, err
	}
	return &xgame.RpcParams{
		MsgName: proto.MessageName(pbMsg),
		Payload: payload,
	}, nil
}
