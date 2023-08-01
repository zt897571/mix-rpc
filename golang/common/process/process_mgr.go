// Package process -----------------------------
// @file      : process_mgr.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/7/29 17:28
// -------------------------------------------

package process

import "sync"

var mgr *ProcessMgr
var once sync.Once

func GetProcessMgr() *ProcessMgr {
	once.Do(func() {
		mgr = &ProcessMgr{}
	})
	return mgr
}

type ProcessMgr struct {
}
