// Package xrpc ----------------------------
// @file      : error_code.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/11 17:45
// -------------------------------------------
package error_code

import "errors"

var TimeOutError = errors.New("timeout")
var ChannelIsFull = errors.New("channel is full")
var ChannelInvalid = errors.New("channel invalid")

var PidFormatError = errors.New("pid format error")

var ProcessCanNotCallSelf = errors.New("process can not call self")
var ProcessNotFound = errors.New("process not found")
var ProcessReplyError = errors.New("process reply error")
var ProcessNotRunning = errors.New("process not running")

var RpcProxyNotFound = errors.New("rpc proxy not found")
var RpcNodeNotConnected = errors.New("host not connected")

var FunctionArgsCountError = errors.New("function agrs count error")
var MfaError = errors.New("mfa error")
var ArgumentError = errors.New("argement error")
var FunctionPanicError = errors.New("function panic")

var PacketFormatError = errors.New("packet format error")
var LogicError = errors.New("logic error")

var NodeNameFormatError = errors.New("node name format error")
var NodeAlreadyInit = errors.New("node already init")
var VerifyError = errors.New("verify cookie error")
var NodeAlreadyConnected = errors.New("node already connected")
