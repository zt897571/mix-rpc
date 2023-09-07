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
	"golang/common/utils"
	xgame "golang/proto"
	"math/rand"
	"sync"
	"testing"
	"time"
)

func TestProcess(t *testing.T) {
	convey.Convey("test process status", t, func() {
		hd := &TestActor{}
		pid, err := GetProcessMgr().CreateProcess(hd)
		convey.ShouldEqual(err, convey.ShouldEqual, nil)
		process := GetProcessMgr().GetProcess(pid)

		convey.ShouldEqual(process.GetStatus(), convey.ShouldEqual, Running)
		process.StopAndWait()
		convey.ShouldEqual(process.GetStatus(), convey.ShouldEqual, Closed)
	})
}

func TestProcess2(t *testing.T) {
	convey.Convey("TestProcess2", t, func() {
		actor1 := &TestActor{test: t}
		actor2 := &TestActor{test: t}

		pid1, err := GetProcessMgr().CreateProcess(actor1)
		process1 := GetProcessMgr().GetProcess(pid1)
		convey.So(process1, convey.ShouldNotEqual, nil)
		convey.So(pid1, convey.ShouldNotEqual, nil)
		convey.So(err, convey.ShouldEqual, nil)

		// test create
		pid2, err := GetProcessMgr().CreateProcess(actor2)
		process2 := GetProcessMgr().GetProcess(pid2)
		convey.So(process2, convey.ShouldNotEqual, nil)
		convey.So(err, convey.ShouldEqual, nil)
		convey.So(pid2, convey.ShouldNotEqual, nil)

		//set target
		actor1.SetTargePid(pid2)
		actor2.SetTargePid(pid1)

		var wg sync.WaitGroup
		wg.Add(2)
		go testCall(pid1, pid2, 10, t, &wg)
		go testCall(pid2, pid1, 10, t, &wg)
		utils.WaitTimeout(&wg, time.Second*10)
	})
}

func testCall(sourcePid iface.IPid, targetPid iface.IPid, times int, t *testing.T, finishWg *sync.WaitGroup) {
	convey.Convey("test call", t, func() {
		process := GetProcessMgr().GetProcess(sourcePid)
		convey.So(process, convey.ShouldNotEqual, nil)

		var wg sync.WaitGroup
		var err error
		wg.Add(times)
		for i := 0; i < times; i++ {
			err = process.asyncRun(func() {
				convey.Convey("test call", t, func() {
					defer func() { wg.Done() }()
					req := &xgame.TestMsg{Rand: rand.Int31()}
					err = process.Cast(targetPid, req)
					convey.So(err, convey.ShouldEqual, nil)
					rst, err := process.Call(targetPid, req, time.Second*1)
					convey.So(err, convey.ShouldEqual, nil)
					convey.So(req, convey.ShouldEqual, rst)
				})
			})
		}
		convey.So(err, convey.ShouldEqual, nil)
		convey.So(utils.WaitTimeout(&wg, time.Second*5), convey.ShouldEqual, false)
		finishWg.Done()
	})
}

func BenchmarkProcess_Call(b *testing.B) {
	actor1 := &TestActor{}
	actor2 := &TestActor{}
	pid1, err := GetProcessMgr().CreateProcess(actor1)
	if err != nil {
		b.FailNow()
	}
	pid2, err := GetProcessMgr().CreateProcess(actor2)
	if err != nil {
		b.FailNow()
	}
	process1 := GetProcessMgr().GetProcess(pid1)
	b.ResetTimer()
	var wg sync.WaitGroup
	wg.Add(b.N)
	for i := 0; i < b.N; i++ {
		err := process1.asyncRun(func() {
			_, err := process1.Call(pid2, &xgame.TestMsg{}, time.Second)
			if err != nil {
				b.FailNow()
				return
			}
			wg.Done()
		})
		if err != nil {
			b.FailNow()
		}
	}
	wg.Wait()
}

var _ iface.IActor = (*TestActor)(nil)

type TestActor struct {
	iface.IProcess
	test      *testing.T
	targetPid iface.IPid
}

func (t *TestActor) SetProcess(process iface.IProcess) {
	t.IProcess = process
}

func (t *TestActor) SetTargePid(pid iface.IPid) {
	t.targetPid = pid
}

func (t *TestActor) OnStart() {
}

func (t *TestActor) OnStop() {
}

func (t *TestActor) HandleCast(from iface.IPid, _ proto.Message) {
	t.checkFrom(from)
}

func (t *TestActor) HandleCall(from iface.IPid, msg proto.Message) (proto.Message, error) {
	t.checkFrom(from)
	return msg, nil
}

func (t *TestActor) checkFrom(from iface.IPid) {
	if t.test != nil {
		convey.Convey("test", t.test, func() {
			convey.So(from, convey.ShouldEqual, t.targetPid)
		})
	}
}
