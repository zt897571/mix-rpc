// Package iface -----------------------------
// @file      : host.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/9/18 18:20
// -------------------------------------------
package iface

import (
	"strings"
)

const nodeNameSep = "@"

var gNode INode

func IsValidNodeName(name string) bool {
	sepLen := len(strings.Split(name, nodeNameSep))
	return sepLen == 3
}

//func GetCookie() string {
//	return gNode.GetCookie()
//}
//
//func GetNodeName() string {
//	return gNode.GetNodeName()
//}
//
//func GetNode() INode {
//	return gNode
//}
//
//func SetNode(node INode) {
//	gNode = node
//}
