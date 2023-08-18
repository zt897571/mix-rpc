// Package error_code -----------------------------
// @file      : error_code.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/11 17:45
// -------------------------------------------
package error_code

import "errors"

var TimeOutError = errors.New("timeout")
var ChannelIsFull = errors.New("channel is full")
