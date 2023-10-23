// Package main -----------------------------
// @file      : node_cmd.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/9/20 19:11
// -------------------------------------------
package main

import (
	"fmt"
	"github.com/gogo/protobuf/proto"
	xgame "golang/proto"
	"golang/xrpc"
)

type NodeCmd struct {
}

func (n *NodeCmd) GetPidList(_ *xgame.ReqGetPidList) (proto.Message, error) {
	xrpc.GetCookie()

	pids := xrpc.GetAllPids()
	var pidsBin [][]byte
	for _, pid := range pids {
		pidsBin = append(pidsBin, pid.Encode())
	}
	return &xgame.ReplyGetPidList{Pids: pidsBin}, nil
}

func (n *NodeCmd) TestNodeCall(msg *xgame.TestMsg) (proto.Message, error) {
	fmt.Printf("recevie node call = %v\n ", msg)
	return msg, nil
}

func (n *NodeCmd) TestNodeCast(msg *xgame.TestMsg) error {
	fmt.Printf("recevie node cast = %v\n ", msg)
	return nil
}
