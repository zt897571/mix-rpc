// Package pid -----------------------------
// @file      : pid_test.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/25 11:00
// -------------------------------------------
package xrpc

import (
	"github.com/smartystreets/goconvey/convey"
	"testing"
)

func TestEPid(t *testing.T) {
	convey.Convey("test erlang pid", t, func() {
		pidBin := []byte{131, 88, 100, 0, 26, 103, 97, 109, 101, 95, 115, 118, 114, 95, 49, 48,
			48, 48, 49, 64, 49, 48, 46, 50, 46, 53, 56, 46, 50, 49, 48, 0, 0, 43, 226,
			0, 0, 0, 3, 100, 222, 209, 114}
		pid, err := DecodePid(pidBin)
		convey.So(err, convey.ShouldEqual, nil)
		convey.So(pid.GetNode(), convey.ShouldEqual, "game_svr_10001@10.2.58.210")
		convey.So(pid.Encode(), convey.ShouldEqual, pidBin)
	})
}

func TestGPid(t *testing.T) {
	convey.Convey("test golang pid", t, func() {
		pid := &gPid{
			id:       10086,
			nodeName: "zhangtuo@10.2.58.210",
		}
		bin := pid.Encode()
		decodePid, err := DecodePid(bin)
		convey.So(err, convey.ShouldEqual, nil)
		convey.So(decodePid, convey.ShouldEqual, pid)
	})
}
