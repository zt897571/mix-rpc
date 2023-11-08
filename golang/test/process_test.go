// Package process -----------------------------
// @file      : process_test.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/1 20:12
// -------------------------------------------
package test

import (
	"github.com/gogo/protobuf/proto"
	"github.com/smartystreets/goconvey/convey"
	"golang/iface"
	xgame "golang/proto"
	"golang/xrpc"
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
		pid1 := prepareActor(t)
		var wg sync.WaitGroup
		goroutineCount := 5
		wg.Add(goroutineCount)
		for i := 0; i < goroutineCount; i++ {
			go testCall(pid1, 500, t, &wg)
		}
		convey.So(xrpc.WaitTimeout(&wg, time.Second*10), convey.ShouldEqual, false)
	})
}

func prepareActor(t *testing.T) iface.IPid {
	actor1 := newTestActor(t)
	pid1, err := xrpc.CreateProcess(actor1)
	process1 := xrpc.GetProcess(pid1)
	convey.So(process1, convey.ShouldNotEqual, nil)
	convey.So(pid1, convey.ShouldNotEqual, nil)
	convey.So(err, convey.ShouldEqual, nil)

	proxyPid, err := xrpc.CreateProcess(xrpc.NewProxyActor(pid1))
	convey.So(err, convey.ShouldEqual, nil)
	convey.So(proxyPid, convey.ShouldNotEqual, nil)
	actor1.SetFromPid(proxyPid)
	return proxyPid
}

func testCall(targetPid iface.IPid, times int, t *testing.T, finishWg *sync.WaitGroup) {
	convey.Convey("rawProcessReqReplyer callRemote", t, func() {
		for i := 0; i < times; i++ {
			req := &xgame.TestMsg{Rand: rand.Int31()}
			rst, err := xrpc.CallTest(targetPid, req, time.Second)
			convey.So(err, convey.ShouldEqual, nil)
			convey.So(req, convey.ShouldEqual, rst)
			err = xrpc.Cast(targetPid, req)
			convey.So(err, convey.ShouldEqual, nil)
		}
		finishWg.Done()
	})
}

func BenchmarkProcess_Call(b *testing.B) {
	actor1 := &testActor{}
	pid1, err := xrpc.CreateProcess(actor1)
	if err != nil {
		b.FailNow()
	}
	proxyPid, err := xrpc.CreateProcess(xrpc.NewProxyActor(pid1))
	if err != nil {
		b.FailNow()
	}
	b.ResetTimer()
	var wg sync.WaitGroup
	wg.Add(1)
	reqMsg := &xgame.TestMsg{Rand: rand.Int31()}
	for i := 0; i < b.N; i++ {
		_, err = xrpc.CallTest(proxyPid, reqMsg, time.Second)
		if err != nil {
			b.FailNow()
		}
	}
	wg.Wait()
}

func TestProcess_Cast(t *testing.T) {
	convey.Convey("rawProcessReqReplyer process cast", t, func() {
		pid1 := prepareActor(t)
		times := 500
		for i := 0; i < times; i++ {
			reqMsg := &xgame.TestMsg{Rand: rand.Int31()}
			rsp, err := xrpc.CallTest(pid1, reqMsg, time.Second)
			convey.So(err, convey.ShouldEqual, nil)
			convey.So(rsp, convey.ShouldEqual, reqMsg)
			convey.So(err, convey.ShouldEqual, nil)
		}
	})
}

var _ iface.IActor = (*testActor)(nil)

type testActor struct {
	iface.IProcess
	test       *testing.T
	formPid    iface.IPid
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

func (t *testActor) SetFromPid(pid iface.IPid) {
	t.formPid = pid
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
			convey.So(from, convey.ShouldEqual, t.formPid)
		})
	}
}

func (t *testActor) getCastResult() proto.Message {
	rest := <-t.castResult
	return rest
}
