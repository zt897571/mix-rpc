// Package process -----------------------------
// @file      : process_test.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/1 20:12
// -------------------------------------------
package xrpc

import (
	"github.com/gogo/protobuf/proto"
	"github.com/smartystreets/goconvey/convey"
	"golang/iface"
	xgame "golang/proto"
	"math/rand"
	"sync"
	"testing"
	"time"
)

//func TestProcess(t *testing.T) {
//	convey.Convey("rawProcessReqReplyer process status", t, func() {
//		hd := &testActor{}
//		pid, err := GetProcessMgr().CreateProcess(hd)
//		convey.ShouldEqual(err, convey.ShouldEqual, nil)
//		process := GetProcessMgr().getProcess(pid)
//
//		convey.ShouldEqual(process.GetStatus(), convey.ShouldEqual, Running)
//		err = process.StopAndWait()
//		convey.ShouldEqual(err, convey.ShouldEqual, nil)
//		convey.ShouldEqual(process.GetStatus(), convey.ShouldEqual, Closed)
//	})
//}

func TestProcessCall(t *testing.T) {
	convey.Convey("TestProcessCall", t, func() {
		pid1, pid2 := prepareActor(t)
		var wg sync.WaitGroup
		goroutineCount := 5
		wg.Add(goroutineCount)
		for i := 0; i < goroutineCount; i++ {
			go testCall(pid1, pid2, 500, t, &wg)
		}
		convey.So(WaitTimeout(&wg, time.Second*10), convey.ShouldEqual, false)
	})
}

func prepareActor(t *testing.T) (iface.IPid, iface.IPid) {
	actor1 := newTestActor(t)
	actor2 := newTestActor(t)

	pid1, err := CreateProcess(actor1)
	process1 := GetProcess(pid1)
	convey.So(process1, convey.ShouldNotEqual, nil)
	convey.So(pid1, convey.ShouldNotEqual, nil)
	convey.So(err, convey.ShouldEqual, nil)

	// rawProcessReqReplyer create
	pid2, err := CreateProcess(actor2)
	process2 := GetProcess(pid2)
	convey.So(process2, convey.ShouldNotEqual, nil)
	convey.So(err, convey.ShouldEqual, nil)
	convey.So(pid2, convey.ShouldNotEqual, nil)

	//set target
	actor1.SetTargePid(pid2)
	actor2.SetTargePid(pid1)
	return pid1, pid2
}

func testCall(sourcePid iface.IPid, targetPid iface.IPid, times int, t *testing.T, finishWg *sync.WaitGroup) {
	convey.Convey("rawProcessReqReplyer callRemote", t, func() {
		process := GetProcess(sourcePid)
		convey.So(process, convey.ShouldNotEqual, nil)

		var wg sync.WaitGroup
		var err error
		wg.Add(times)
		for i := 0; i < times; i++ {
			err = process.asyncRun(func() {
				convey.Convey("rawProcessReqReplyer callRemote", t, func() {
					defer wg.Done()
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
		convey.So(WaitTimeout(&wg, time.Second*5), convey.ShouldEqual, false)
		finishWg.Done()
	})
}

func BenchmarkProcess_Call(b *testing.B) {
	actor1 := &testActor{}
	actor2 := &testActor{}
	pid1, err := CreateProcess(actor1)
	if err != nil {
		b.FailNow()
	}
	pid2, err := CreateProcess(actor2)
	if err != nil {
		b.FailNow()
	}
	process1 := GetProcess(pid1)
	b.ResetTimer()
	var wg sync.WaitGroup
	wg.Add(1)
	err = process1.asyncRun(func() {
		defer wg.Done()
		for i := 0; i < b.N; i++ {
			_, err := process1.Call(pid2, &xgame.TestMsg{}, time.Second)
			if err != nil {
				b.FailNow()
				return
			}
		}
	})
	if err != nil {
		b.FailNow()
	}
	wg.Wait()
}

func TestProcess_Cast(t *testing.T) {
	convey.Convey("rawProcessReqReplyer process cast", t, func() {
		pid1, pid2 := prepareActor(t)
		process1 := GetProcess(pid1)
		actor := GetProcess(pid2).GetActor().(*testActor)
		times := 500
		var wg sync.WaitGroup
		wg.Add(times)
		for i := 0; i < times; i++ {
			err := process1.asyncRun(func() {
				convey.Convey("rawProcessReqReplyer cast", t, func() {
					reqMsg := &xgame.TestMsg{Rand: rand.Int31()}
					err := process1.Cast(pid2, reqMsg)
					convey.So(err, convey.ShouldEqual, nil)
					rst := actor.getCastResult()
					convey.So(rst, convey.ShouldEqual, reqMsg)
					wg.Done()
				})
			})
			convey.So(err, convey.ShouldEqual, nil)
		}
		convey.So(WaitTimeout(&wg, time.Second*10), convey.ShouldEqual, false)
	})
}

var _ iface.IActor = (*testActor)(nil)

type testActor struct {
	iface.IProcess
	test       *testing.T
	targetPid  iface.IPid
	castResult chan proto.Message
}

func newTestActor(t *testing.T) *testActor {
	return &testActor{
		test:       t,
		castResult: make(chan proto.Message, 10000),
	}
}

func (t *testActor) SetProcess(process iface.IProcess) {
	t.IProcess = process
}

func (t *testActor) SetTargePid(pid iface.IPid) {
	t.targetPid = pid
}

func (t *testActor) OnStart() {
}

func (t *testActor) OnStop() {
}

func (t *testActor) HandleCast(from iface.IPid, msg proto.Message) {
	t.checkFrom(from)
	t.castResult <- msg
}

func (t *testActor) HandleCall(from iface.IPid, msg proto.Message) (proto.Message, error) {
	t.checkFrom(from)
	return msg, nil
}

func (t *testActor) checkFrom(from iface.IPid) {
	if t.test != nil {
		convey.Convey("rawProcessReqReplyer", t.test, func() {
			convey.So(from, convey.ShouldEqual, t.targetPid)
		})
	}
}

func (t *testActor) getCastResult() proto.Message {
	rest := <-t.castResult
	return rest
}
