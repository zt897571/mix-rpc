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
	"golang/common/iface"
	utils "golang/common/util"
	xgame "golang/proto"
	"math/rand"
	"sync"
	"testing"
	"time"
)

var server *RpcServer
var handler *TestNodeHandler

func init() {
	handler = newTestNodeHandler()
	GetRpcProxyMgr().RegisterNodeMsg("xgame.test_msg", handler)
	server = NewRpcServer(8000)
	go server.Start()
}

type TestNodeHandler struct {
	msgChan chan proto.Message
}

func newTestNodeHandler() *TestNodeHandler {
	return &TestNodeHandler{
		msgChan: make(chan proto.Message, 1000),
	}
}

func (t *TestNodeHandler) NodeCall(_ iface.IRpcProxy, message proto.Message) (proto.Message, error) {
	return message, nil
}

func (t *TestNodeHandler) NodeCast(_ iface.IRpcProxy, message proto.Message) error {
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
		reqMsg := &xgame.TestMsg{Msg: "testing"}
		client, err := Connect("localhost:8000")
		rsp, err := client.NodeCall(reqMsg, time.Second*5)
		convey.So(err, convey.ShouldEqual, nil)
		convey.So(rsp, convey.ShouldNotEqual, nil)
		convey.So(rsp, convey.ShouldEqual, reqMsg)
	})
}

func TestNodeBatchCall(t *testing.T) {
	convey.Convey("test batch node call", t, func() {
		size := 1000
		bm := utils.NewBatchMgr(100)
		resultChan := make(chan []proto.Message, size)
		for i := 0; i < size; i++ {
			bm.AddTask(func() {
				ct, err := Connect("localhost:8000")
				if err != nil {
					return
				}
				reqMsg := &xgame.TestMsg{Msg: "testing", Rand: rand.Int31()}
				rsp, err := ct.NodeCall(reqMsg, time.Second*3)
				if err != nil {
					return
				}
				resultChan <- []proto.Message{reqMsg, rsp}
			})
		}
		bm.Exec()
		close(resultChan)
		var result [][]proto.Message
		for c := range resultChan {
			result = append(result, c)
			convey.So(len(c), convey.ShouldEqual, 2)
			convey.So(c[0], convey.ShouldEqual, c[1])
		}
		convey.So(len(result), convey.ShouldEqual, size)
	})
}

func TestNodeCast(t *testing.T) {
	convey.Convey("test node cast", t, func() {
		ct, err := Connect("localhost:8000")
		convey.So(err, convey.ShouldEqual, nil)
		reqMsg := &xgame.TestMsg{Msg: "testing", Rand: rand.Int31()}
		err = ct.NodeCast(reqMsg)
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
				ct, err := Connect("localhost:8000")
				if err != nil {
					return
				}
				reqMsg := &xgame.TestMsg{Msg: "testing", Rand: rand.Int31()}
				err = ct.NodeCast(reqMsg)
			})
		}
		bm.Exec()
		var result []proto.Message
		var wg sync.WaitGroup
		wg.Add(1)
		go func() {
			for i := 0; i < size; i++ {
				result = append(result, handler.WaitResponse())
			}
			wg.Done()
		}()
		go func() {
			<-time.After(time.Second * 5)
			wg.Done()
		}()
		wg.Wait()
		convey.So(len(result), convey.ShouldEqual, size)
	})
}
