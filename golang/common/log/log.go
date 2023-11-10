// Package log -----------------------------
// @file      : log.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/11/10 12:02
// -------------------------------------------
package log

type ILog interface {
	Info(msg string)
	Error(msg string)
	Debugf(fmts string, args ...interface{})
	Infof(fmts string, args ...interface{})
	Warnf(fmts string, args ...interface{})
	Errorf(fmts string, args ...interface{})
}

var glog ILog = &defaultLogger{}

func RegisterLog(log ILog) {
	glog = log
}

func Info(msg string) {
	glog.Info(msg)
}

func Error(msg string) {
	glog.Error(msg)
}

func Debugf(fmts string, args ...interface{}) {
	glog.Debugf(fmts, args...)
}

func Infof(fmts string, args ...interface{}) {
	glog.Infof(fmts, args...)
}

func Warnf(fmts string, args ...interface{}) {
	glog.Warnf(fmts, args...)
}

func Errorf(fmts string, args ...interface{}) {
	glog.Errorf(fmts, args...)
}
