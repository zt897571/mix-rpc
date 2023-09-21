// Package rpc -----------------------------
// @file      : packet_util.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/7/31 9:55
// -------------------------------------------
package rpc

import (
	"github.com/gogo/protobuf/proto"
	"golang/common/iface"
	xgame "golang/proto"
	"reflect"
)

const (
	REQ_FLAG     iface.FlagType = 0b1
	CALL_FLAG                   = 0b10
	NODEMSG_FLAG                = 0b100
)

func BuildFlag(flagList []iface.FlagType) iface.FlagType {
	var result iface.FlagType
	for _, f := range flagList {
		result = f ^ result
	}
	return result
}

func CheckFlag(flag iface.FlagType, flagType iface.FlagType) bool {
	return flag&flagType == flagType
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

func BuildRpcResult(msg proto.Message, err error) *xgame.RpcResult {
	rst := &xgame.RpcResult{}
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
