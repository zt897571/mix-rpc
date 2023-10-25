// Package rpc -----------------------------
// @file      : packet_test.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/7/31 10:25
// -------------------------------------------
package xrpc

import (
	"github.com/gogo/protobuf/proto"
	xgame "golang/proto"
	"testing"
)
import . "github.com/smartystreets/goconvey/convey"

func TestFlag(t *testing.T) {
	Convey("test flag", t, func() {
		flag := buildFlag([]flagType{REQ_FLAG, CALL_FLAG})
		So(checkFlag(flag, REQ_FLAG), ShouldEqual, true)
		So(checkFlag(flag, CALL_FLAG), ShouldEqual, true)
		So(checkFlag(flag, NODEMSG_FLAG), ShouldEqual, false)
	})
}

func TestPacket(t *testing.T) {
	Convey("test packet", t, func() {
		//encode
		flag := buildFlag([]flagType{REQ_FLAG, CALL_FLAG})
		testMsg := &xgame.TestMsg{Rand: 2023}
		var seq uint32 = 10086
		msgBin, err := buildMsg(flag, seq, testMsg)
		So(err, ShouldEqual, nil)

		pkt, err := newPacket(msgBin)
		So(err, ShouldEqual, nil)
		So(pkt.seq, ShouldEqual, seq)
		So(pkt.flag, ShouldEqual, flag)

		msg := &xgame.TestMsg{}
		err = proto.Unmarshal(pkt.payload, msg)
		So(err, ShouldEqual, nil)
		So(msg, ShouldEqual, testMsg)
	})
}
