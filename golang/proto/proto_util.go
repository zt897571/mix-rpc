// Package xgame -----------------------------
// @file      : proto_util.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/2 14:53
// -------------------------------------------
package xgame

import (
	"github.com/gogo/protobuf/proto"
)

// todo auto generate

var id2Msg = map[int32]func() proto.Message{1: func() proto.Message { return &TestMsg{} }}

func GetMsgById(msgId int32) proto.Message {
	return id2Msg[msgId]()
}
