// Package process -----------------------------
// @file      : process_test.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/1 20:12
// -------------------------------------------
package process

type TestProcessHandler struct {
	status int32
}

func (t *TestProcessHandler) OnStart() {
	t.status = 1
}

func (t *TestProcessHandler) OnStop() {
	t.status = 2
}

//func TestProcess(t *testing.T) {
//	convey.Convey("TestProcess", t, func() {
//		hd := &TestProcessHandler{}
//		pid, err := GetProcessMgr().CreateProcess(hd)
//		convey.ShouldEqual(err, convey.ShouldEqual, nil)
//		convey.ShouldEqual(hd, convey.ShouldEqual, 1)
//		process := GetProcessMgr().GetProcess(pid)
//
//		convey.ShouldEqual(process.GetStatus(), convey.ShouldEqual, Running)
//		process.StopAndWait()
//		convey.ShouldEqual(process.GetStatus(), convey.ShouldEqual, Closed)
//		convey.ShouldEqual(hd, convey.ShouldEqual, 2)
//	})
//}
