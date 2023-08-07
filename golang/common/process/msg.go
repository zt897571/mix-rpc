// Package process -----------------------------
// @file      : msg.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/1 19:44
// -------------------------------------------
package process

type processMsg struct {
	callback func()
}
