// Package main -----------------------------
// @file      : node_cmd.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/9/20 19:11
// -------------------------------------------
package main

import (
	"github.com/gogo/protobuf/proto"
	"golang/common/log"
	xgame "golang/proto"
	"golang/xrpc"
)

type NodeCmd struct {
}

func (n *NodeCmd) GetPidList(_ *xgame.ReqGetPidList) (proto.Message, error) {
	pids := xrpc.GetAllPids()
	if len(pids) == 0 {
		_, err := xrpc.CreateProcess(&TestActor{})
		if err != nil {
			return nil, err
		}
	}
	pids = xrpc.GetAllPids()
	var pidsBin [][]byte
	for _, pid := range pids {
		pidsBin = append(pidsBin, pid.Encode())
	}
	log.Infof("receive GetPidList")
	return &xgame.ReplyGetPidList{Pids: pidsBin}, nil
}

func (n *NodeCmd) TestNodeCall(msg *xgame.TestMsg) (proto.Message, error) {
	log.Infof("recevie node call = %v\n ", msg)
	return msg, nil
}

func (n *NodeCmd) TestNodeCast(msg *xgame.TestMsg) error {
	log.Infof("recevie node cast = %v\n ", msg)
	return nil
}
