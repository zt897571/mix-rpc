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
	iface2 "golang/iface"
	xgame "golang/proto"
	"golang/xrpc"
)

func init() {
	nodeCmd := &NodeCmd{}
	xrpc.RegisterNodeMsg(nodeCmd.GetModuleName(), nodeCmd)
}

type NodeCmd struct {
}

var _ xrpc.ITestNodeServiceHandler = (*NodeCmd)(nil)
var _ iface2.IProtoMsgHandler = (*NodeCmd)(nil)

func (n *NodeCmd) GetModuleName() string {
	return "NodeCmd"
}

func (n *NodeCmd) OnCallNodeTest(msg *xgame.TestMsg) (*xgame.TestMsg, error) {
	return msg, nil
}

func (n *NodeCmd) OnCastNodeTest(msg *xgame.TestMsg) error {
	return nil
}

func (n *NodeCmd) OnCallNodeGetPidList(req *xgame.ReqGetPidList) (*xgame.ReplyGetPidList, error) {
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

func (n *NodeCmd) OnCastNodeGetPidList(list *xgame.ReqGetPidList) error {
	return nil
}

func (n *NodeCmd) HandleCallMsg(reqName string, msg proto.Message) (proto.Message, error) {
	log.Infof("receive call msg %s", reqName)
	return xrpc.DispatchTestNodeServiceCallMsg(n, reqName, msg)
}

func (n *NodeCmd) HandleCastMsg(reqName string, msg proto.Message) error {
	log.Infof("receive cast msg %s", reqName)
	return xrpc.DispatchTestNodeServiceCastMsg(n, reqName, msg)
}
