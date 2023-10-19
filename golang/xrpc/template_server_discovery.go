// Package xrpc -----------------------------
// @file      : template_server_discovery.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/10/19 18:40
// -------------------------------------------
package xrpc

import (
	"fmt"
	"strings"
)

// zhangtuo 临时处理, 后期替换成epmd 或者 etcd
type nemplateServerDiscovery struct {
}

func newTemplateServerDiscovery() *nemplateServerDiscovery {
	return &nemplateServerDiscovery{}
}

func (t nemplateServerDiscovery) GetIpAddressByNode(s string) (string, error) {
	sep := strings.Split(s, nodenameSep)
	return fmt.Sprintf("%s:%s", sep[1], sep[2]), nil
}
