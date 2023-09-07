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

var PidFormatError = errors.New("pid format error")
var PidDecoderNotFound = errors.New("can not found pid decorder")

var ProcessCanNotCallSelf = errors.New("process can not call self")
var ProcessNotFound = errors.New("process not found")

var RpcProxyNotFound = errors.New("rpc proxy not found")
var RpcHostNotConnected = errors.New("host not connected")
