// Package utils -----------------------------
// @file      : util_test.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/9/4 11:54
// -------------------------------------------
package utils

import (
	"github.com/smartystreets/goconvey/convey"
	"testing"
)

type TestSt struct {
}

func (t *TestSt) Add(a int32, b int32) int32 {
	return a + b
}

func (t *TestSt) Get(n int32) int32 {
	return n
}

func (t *TestSt) get(n int32) int32 {
	return n
}

func TestScanFunction(t *testing.T) {
	convey.Convey("test scan function", t, func() {
		fs := ScanFunction(&TestSt{})
		convey.So(len(fs), convey.ShouldEqual, 2)
		rst, err := fs[0].SafeCall(1, 2)
		convey.So(err, convey.ShouldEqual, nil)
		convey.So(rst, convey.ShouldEqual, 3)
	})
}
