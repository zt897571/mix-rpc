// Package utils -----------------------------
// @file      : util_test.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/9/4 11:54
// -------------------------------------------
package utils

import (
	xgame "golang/proto"
	"testing"
)

type TestSt struct {
}

func (t *TestSt) HandleCall(msg *xgame.TestMsg) error {
	return nil
}

func (t *TestSt) handleCast() {}

func TestScanFunction(t *testing.T) {
	ScanStruct(&TestSt{})
}
