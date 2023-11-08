// Package xrpc -----------------------------
// @file      : node_msg_test.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/14 12:06
// -------------------------------------------
package test

import (
	"github.com/gogo/protobuf/proto"
	"github.com/smartystreets/goconvey/convey"
	"golang/common/utils"
	xgame "golang/proto"
	"golang/xrpc"
	"math/rand"
	"testing"
	"time"
)

var handler *TestNodeHandler
var testCookie = "testcookie"
var nodeName = "testnode1@localhost:8000"

func init() {
	handler = newTestNodeHandler()
	xrpc.RegisterNodeMsg("test", handler)
	err := xrpc.Start(nodeName, testCookie)
	if err != nil {
		return
	}
	err = xrpc.Connect(nodeName)
	if err != nil {
		return
	}
}

type TestNodeHandler struct {
	msgChan chan proto.Message
}

func newTestNodeHandler() *TestNodeHandler {
	return &TestNodeHandler{
		msgChan: make(chan proto.Message, 1000),
	}
}

func (t *TestNodeHandler) HandleCallMsg(reqName string, msg proto.Message) (proto.Message, error) {
	return xrpc.DispatchTestNodeServiceCallMsg(t, reqName, msg)
}

func (t *TestNodeHandler) HandleCastMsg(reqName string, msg proto.Message) error {
	return xrpc.DispatchTestNodeServiceCastMsg(t, reqName, msg)
}

func (t *TestNodeHandler) GetModuleName() string {
	return "test"
}

func (t *TestNodeHandler) OnCallNodeTest(msg *xgame.TestMsg) (*xgame.TestMsg, error) {
	return msg, nil
}

func (t *TestNodeHandler) OnCastNodeTest(msg *xgame.TestMsg) error {
	t.msgChan <- msg
	return nil
}

func (t *TestNodeHandler) OnCallNodeGetPidList(list *xgame.ReqGetPidList) (*xgame.ReplyGetPidList, error) {
	return nil, nil
}

func (t *TestNodeHandler) OnCastNodeGetPidList(list *xgame.ReqGetPidList) error {
	return nil
}

func (t *TestNodeHandler) WaitResponse() proto.Message {
	return <-t.msgChan
}

func TestConnect(t *testing.T) {
	convey.Convey("test connect", t, func() {
		err := xrpc.Connect(nodeName)
		convey.So(err, convey.ShouldEqual, nil)
	})
}

func TestNodeCall(t *testing.T) {
	convey.Convey("test node call", t, func() {
		reqMsg := &xgame.TestMsg{Msg: "testing", Rand: 123}
		rsp, err := xrpc.CallNodeTest(nodeName, "test", reqMsg, time.Second*5)
		convey.So(err, convey.ShouldEqual, nil)
		convey.So(rsp, convey.ShouldEqual, reqMsg)
	})
}

func BenchmarkNodeCallTest_10(b *testing.B) {
	benchNodeCall(b, randomByte(10))
}

func BenchmarkNodeCallTest_100(b *testing.B) {
	benchNodeCall(b, randomByte(100))
}

func BenchmarkNodeCallTest_1000(b *testing.B) {
	benchNodeCall(b, randomByte(1000))
}

func benchNodeCall(b *testing.B, data []byte) {
	for n := 0; n < b.N; n++ {
		reqMsg := &xgame.TestMsg{Msg: "testing", TestBt: data, Rand: rand.Int31()}
		_, err := xrpc.CallNodeTest(nodeName, "test", reqMsg, time.Second*5)
		if err != nil {
			b.FailNow()
		}
	}
}

func TestNodeBatchCall(t *testing.T) {
	convey.Convey("test batch node call", t, func() {
		size := 1000
		bm := utils.NewBatchMgr(10)
		for i := 0; i < size; i++ {
			bm.AddTask(func() {
				convey.Convey("test call ", t, func() {
					reqMsg := &xgame.TestMsg{Msg: "testing", Rand: rand.Int31(), TestBt: randomByte(100)}
					rsp, err := xrpc.CallNodeTest(nodeName, "test", reqMsg, time.Second*5)
					convey.So(err, convey.ShouldEqual, nil)
					convey.So(rsp, convey.ShouldEqual, reqMsg)
				})
			})
		}
		bm.Exec()
	})
}

func TestNodeCast(t *testing.T) {
	convey.Convey("test node cast", t, func() {
		reqMsg := &xgame.TestMsg{Msg: "testing", Rand: rand.Int31()}
		err := xrpc.CastNodeTest(nodeName, "test", reqMsg)
		convey.So(err, convey.ShouldEqual, nil)
		convey.So(handler.WaitResponse(), convey.ShouldEqual, reqMsg)
	})
}

func TestNodeSeqCast(t *testing.T) {
	convey.Convey("test node seq cast", t, func() {
		bm := utils.NewBatchMgr(100)
		size := 1000
		for i := 0; i < size; i++ {
			bm.AddTask(func() {
				convey.Convey("test cast ", t, func() {
					reqMsg := &xgame.TestMsg{Msg: "testing", Rand: rand.Int31()}
					err := xrpc.CastNodeTest(nodeName, "test", reqMsg)
					convey.So(err, convey.ShouldEqual, nil)
				})
			})
		}
		bm.Exec()
		var result []proto.Message
		defer func() {
			convey.So(len(result), convey.ShouldEqual, size)
		}()
		for {
			select {
			case msg := <-handler.msgChan:
				result = append(result, msg)
			case <-time.After(time.Second * 5):
				return
			}
		}
	})
}

func randomByte(count int32) []byte {
	data := make([]byte, count)
	rand.Read(data)
	return data
}
