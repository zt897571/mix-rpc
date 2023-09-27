// Package main -----------------------------
// @file      : test_actor.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/9/26 19:37
// -------------------------------------------
package main

import (
	"github.com/gogo/protobuf/proto"
	"golang/common/iface"
	"golang/common/log"
	"golang/common/process"
)

var pidList []iface.IPid

func createProcess([]string) error {
	pid, err := process.GetProcessMgr().CreateProcess(&TestActor{})
	if err != nil {
		return err
	}
	pidList = append(pidList, pid)
	return nil
}

type TestActor struct {
	iface.IProcess
}

func (t *TestActor) SetProcess(process iface.IProcess) {
	t.IProcess = process
}

func (t *TestActor) OnStart() {
	log.Infof("actor start %s\n", t.GetPid())
}

func (t *TestActor) OnStop() {
	log.Infof("actor stop %s\n", t.GetPid())
}

func (t *TestActor) HandleCast(from iface.IPid, msg proto.Message) {
	log.Infof("Receive Actor Cast from = %v, msg = %v", from, msg)
}

func (t *TestActor) HandleCall(from iface.IPid, msg proto.Message) (proto.Message, error) {
	log.Infof("Receive Actor Call from = %v, msg = %v", from, msg)
	return msg, nil
}
