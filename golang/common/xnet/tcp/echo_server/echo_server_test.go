// Package tcp -----------------------------
// @file      : tcp_test.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/7 18:14
// -------------------------------------------
package echo_server

import (
	"fmt"
	"github.com/smartystreets/goconvey/convey"
	"testing"
	"time"
)

var echoServer *EchoServer
var ipaddress = fmt.Sprintf("localhost:%d", 9000)

func init() {
	echoServer = NewEchoServer(ipaddress)
	err := echoServer.Start()
	if err != nil {
		return
	}
}

func TestClient_Connect(t *testing.T) {
	convey.Convey("test connect", t, func() {
		err, _ := connect(ipaddress, nil)
		convey.So(err, convey.ShouldEqual, nil)
	})
}

func TestSendMsg(t *testing.T) {
	convey.Convey("test send msg", t, func() {
		reqMsg := "testMsg"
		err, client := connect(ipaddress, nil)
		convey.So(err, convey.ShouldEqual, nil)
		err = client.SendMsg(reqMsg)
		convey.So(err, convey.ShouldEqual, nil)
		response, err := client.WaitResponse(time.Second)
		convey.So(response, convey.ShouldEqual, reqMsg)
		convey.So(err, convey.ShouldEqual, nil)
	})
}

func TestSeqSendMsg(t *testing.T) {
	convey.Convey("test send msg", t, func() {
		reqMsg := "testMsg"
		err, client := connect(ipaddress, nil)
		convey.So(err, convey.ShouldEqual, nil)
		for i := 0; i < 10; i++ {
			msg := fmt.Sprintf("%s-%d", reqMsg, i)
			err = client.SendMsg(msg)
			convey.So(err, convey.ShouldEqual, nil)
			response, err := client.WaitResponse(time.Second)
			convey.So(err, convey.ShouldEqual, nil)
			convey.So(response.(string), convey.ShouldEqual, msg)
		}
		for i := 0; i < 100; i++ {
			msg := fmt.Sprintf("%s-%d", reqMsg, i)
			err = client.SendMsg(msg)
			convey.So(err, convey.ShouldEqual, nil)
		}
		for i := 0; i < 100; i++ {
			msg := fmt.Sprintf("%s-%d", reqMsg, i)
			response, err := client.WaitResponse(time.Second)
			convey.So(err, convey.ShouldEqual, nil)
			convey.So(response.(string), convey.ShouldEqual, msg)
		}
	})
}

func TestClient_close(t *testing.T) {
	convey.Convey("test close", t, func() {
		err, client := connect(ipaddress, nil)
		convey.So(err, convey.ShouldEqual, nil)
		client.conn.Close()
		response, err := client.WaitResponse(time.Second * 5)
		convey.So(err, convey.ShouldEqual, nil)
		convey.So(response.(string), convey.ShouldEqual, CLOSE_MSG)
		convey.So(err, convey.ShouldEqual, nil)
	})
}
