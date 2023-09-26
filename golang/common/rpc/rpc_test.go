// Package rpc -----------------------------
// @file      : node_msg_test.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/14 12:06
// -------------------------------------------
package rpc

import (
	"github.com/gogo/protobuf/proto"
	"github.com/smartystreets/goconvey/convey"
	iface2 "golang/common/iface"
	"golang/common/utils"
	xgame "golang/proto"
	"math/rand"
	"testing"
	"time"
)

var server *RpcServer
var handler *TestNodeHandler
var proxy iface2.IRpcProxy

func init() {
	handler = newTestNodeHandler()
	RegisterNodeMsg("test", handler)
	server = NewRpcServer("0.0.0.0:8000")
	err := server.Start()
	if err != nil {
		return
	}
	p, err := Connect("localhost:8000")
	if err != nil {
		return
	}
	proxy = p
}

type TestNodeHandler struct {
	msgChan chan proto.Message
}

func newTestNodeHandler() *TestNodeHandler {
	return &TestNodeHandler{
		msgChan: make(chan proto.Message, 1000),
	}
}

func (t *TestNodeHandler) NodeCall(message proto.Message) (proto.Message, error) {
	return message, nil
}

func (t *TestNodeHandler) NodeCast(message proto.Message) error {
	t.msgChan <- message
	return nil
}

func (t *TestNodeHandler) WaitResponse() proto.Message {
	return <-t.msgChan
}

func TestConnect(t *testing.T) {
	convey.Convey("test connect", t, func() {
		client, err := Connect("localhost:8000")
		convey.So(err, convey.ShouldEqual, nil)
		convey.So(client, convey.ShouldNotEqual, nil)
	})
}

func TestNodeCall(t *testing.T) {
	convey.Convey("test node call", t, func() {
		reqMsg := &xgame.TestMsg{Msg: "testing", Rand: 123}
		proxy, err := Connect("localhost:8000")
		mfa, err := BuildMfa("test", "NodeCall", reqMsg)
		convey.So(err, convey.ShouldEqual, nil)
		convey.So(mfa, convey.ShouldNotEqual, nil)
		rsp, err := NodeCall(proxy, mfa, time.Second*5)
		convey.So(err, convey.ShouldEqual, nil)
		convey.So(rsp, convey.ShouldEqual, reqMsg)
	})
}

func BenchmarkNodeCallTest_10(b *testing.B) {
	benchNodeCall(b, utils.RandomByte(10))
}

func BenchmarkNodeCallTest_100(b *testing.B) {
	benchNodeCall(b, utils.RandomByte(100))
}

func BenchmarkNodeCallTest_1000(b *testing.B) {
	benchNodeCall(b, utils.RandomByte(1000))
}

func benchNodeCall(b *testing.B, data []byte) {
	for n := 0; n < b.N; n++ {
		reqMsg := &xgame.TestMsg{Msg: "testing", TestBt: data, Rand: rand.Int31()}
		mfa, err := BuildMfa("test", "NodeCall", reqMsg)
		if err != nil {
			b.FailNow()
		}
		_, err = NodeCall(proxy, mfa, time.Second*3)
		if err != nil {
			b.FailNow()
		}
	}
}

func TestNodeBatchCall(t *testing.T) {
	convey.Convey("test batch node call", t, func() {
		size := 1000
		bm := utils.NewBatchMgr(10)
		proxy, err := Connect("localhost:8000")
		convey.So(err, convey.ShouldEqual, nil)
		for i := 0; i < size; i++ {
			bm.AddTask(func() {
				convey.Convey("test call ", t, func() {
					reqMsg := &xgame.TestMsg{Msg: "testing", Rand: rand.Int31(), TestBt: utils.RandomByte(100)}
					mfa, err := BuildMfa("test", "NodeCall", reqMsg)
					convey.So(err, convey.ShouldEqual, nil)
					convey.So(mfa, convey.ShouldNotEqual, nil)
					rsp, err := NodeCall(proxy, mfa, time.Second*3)
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
		proxy, err := Connect("localhost:8000")
		convey.So(err, convey.ShouldEqual, nil)
		reqMsg := &xgame.TestMsg{Msg: "testing", Rand: rand.Int31()}
		mfa, err := BuildMfa("test", "NodeCast", reqMsg)
		convey.So(err, convey.ShouldEqual, nil)
		convey.So(mfa, convey.ShouldNotEqual, nil)
		err = NodeCast(proxy, mfa)
		convey.So(err, convey.ShouldEqual, nil)
		convey.So(handler.WaitResponse(), convey.ShouldEqual, reqMsg)
	})
}

func TestNodeSeqCast(t *testing.T) {
	convey.Convey("test node seq cast", t, func() {
		bm := utils.NewBatchMgr(100)
		size := 1000
		proxy, err := Connect("localhost:8000")
		convey.So(err, convey.ShouldEqual, nil)
		for i := 0; i < size; i++ {
			bm.AddTask(func() {
				convey.Convey("test cast ", t, func() {
					reqMsg := &xgame.TestMsg{Msg: "testing", Rand: rand.Int31()}
					mfa, err := BuildMfa("test", "NodeCast", reqMsg)
					convey.So(err, convey.ShouldEqual, nil)
					convey.So(mfa, convey.ShouldNotEqual, nil)
					err = NodeCast(proxy, mfa)
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
