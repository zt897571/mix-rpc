// Package rpc -----------------------------
// @file      : packet_test.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/7/31 10:25
// -------------------------------------------
package rpc

import (
	"testing"
)
import . "github.com/smartystreets/goconvey/convey"

func TestPacket(t *testing.T) {
	Convey("test flag", t, func() {
		flag := BuildFlag([]FlagType{REQ_FLAG, CALL_FLAG})
		So(CheckFlag(flag, REQ_FLAG), ShouldEqual, true)
		So(CheckFlag(flag, CALL_FLAG), ShouldEqual, true)
		So(CheckFlag(flag, NODEMSG_FLAG), ShouldEqual, false)
	})
}
