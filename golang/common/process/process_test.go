// Package process -----------------------------
// @file      : process_test.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/1 20:12
// -------------------------------------------
package process

import (
	"github.com/gogo/protobuf/proto"
	"github.com/smartystreets/goconvey/convey"
	"golang/common/iface"
	"testing"
)

func TestProcess(t *testing.T) {
	convey.Convey("TestProcess", t, func() {
		hd := &TestProcessHandler{}
		pid, err := GetProcessMgr().CreateProcess(hd)
		convey.ShouldEqual(err, convey.ShouldEqual, nil)
		convey.ShouldEqual(hd, convey.ShouldEqual, 1)
		process := GetProcessMgr().GetProcess(pid)

		convey.ShouldEqual(process.GetStatus(), convey.ShouldEqual, Running)
		process.StopAndWait()
		convey.ShouldEqual(process.GetStatus(), convey.ShouldEqual, Closed)
		convey.ShouldEqual(hd, convey.ShouldEqual, 2)
	})
}

var _ iface.IProcessMsgHandler = (*TestProcessHandler)(nil)

type TestProcessHandler struct {
	status int32
}

func (t *TestProcessHandler) HandleCast(pb proto.Message) {
}

func (t *TestProcessHandler) HandleCall(pid iface.IPid, pb proto.Message) (proto.Message, error) {
	return nil, nil
}

func (t *TestProcessHandler) SetProcess(process iface.IProcess) {
}

func (t *TestProcessHandler) OnStart() {
	t.status = 1
}

func (t *TestProcessHandler) OnStop() {
	t.status = 2
}
