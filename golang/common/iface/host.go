// Package iface -----------------------------
// @file      : host.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/9/18 18:20
// -------------------------------------------
package iface

import (
	"golang/common/error_code"
	"strings"
)

const nodeNameSep = "@"

var gNodeName NodeName

func GetNodeName() NodeName {
	return gNodeName
}

func SetNodeName(nodeName string) error {
	if !IsValidNodeName(nodeName) {
		return error_code.NodeNameFormatError
	}
	gNodeName = NodeName(nodeName)
	return nil
}

type NodeName string

func IsValidNodeName(name string) bool {
	return len(strings.Split(name, nodeNameSep)) == 3
}

func (n NodeName) GetHost() string {
	sp := strings.Split(string(n), nodeNameSep)
	return sp[1]
}

func (n NodeName) GetName() string {
	sp := strings.Split(string(n), nodeNameSep)
	return sp[0]
}

func (n NodeName) String() string {
	return string(n)
}
