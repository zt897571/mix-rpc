// Package iface -----------------------------
// @file      : host.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/9/18 18:20
// -------------------------------------------
package iface

var host string

func GetHost() string {
	return host
}

func SetHost(h string) {
	host = h
}
