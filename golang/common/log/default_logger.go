// Package log -----------------------------
// @file      : default_logger.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/11/10 12:15
// -------------------------------------------
package log

import "fmt"

type defaultLogger struct{}

var _ ILog = (*defaultLogger)(nil)

func (d *defaultLogger) Info(msg string) {
	fmt.Println(msg)
}

func (d *defaultLogger) Error(msg string) {
	fmt.Println(msg)
}

func (d *defaultLogger) Debugf(fmts string, args ...interface{}) {
	fmt.Println(fmt.Sprintf(fmts, args...))
}

func (d *defaultLogger) Infof(fmts string, args ...interface{}) {
	fmt.Println(fmt.Sprintf(fmts, args...))
}

func (d *defaultLogger) Warnf(fmts string, args ...interface{}) {
	fmt.Println(fmt.Sprintf(fmts, args...))
}

func (d *defaultLogger) Errorf(fmts string, args ...interface{}) {
	fmt.Println(fmt.Sprintf(fmts, args...))
}
