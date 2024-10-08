// Package xrpc -----------------------------
// @file      : gpid.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/24 13:00
// golang pid 代码实现
// format
// [magic num](1), [id](4), [length](2)[hoststring]
// -------------------------------------------
package xrpc

import (
	"encoding/binary"
	"fmt"
	"golang/error_code"
	"golang/iface"
	"sync/atomic"
)

type gPid struct {
	nodeName string
	id       uint32
	//time     uint64  todo:: zhangtuo fix this
}

var _ iface.IPid = (*gPid)(nil)

func (g *gPid) String() string {
	return fmt.Sprintf("%s:%d", g.nodeName, g.id)
}

func (g *gPid) GetNode() string {
	return g.nodeName
}

func (g *gPid) IsLocal() bool {
	return g.nodeName == GetNodeName()
}

func (g *gPid) Encode() []byte {
	host := g.nodeName
	bin := make([]byte, 7+len(g.nodeName))
	bin[0] = goPidProto
	binary.BigEndian.PutUint32(bin[1:5], g.id)
	binary.BigEndian.PutUint16(bin[5:7], uint16(len(host)))
	for i, b := range []byte(g.nodeName) {
		bin[7+i] = b
	}
	return bin
}

var baseIndex uint32

func nextId() uint32 {
	return atomic.AddUint32(&baseIndex, 1)
}

func isGPid(bytes []byte) bool {
	return len(bytes) >= 7 && bytes[0] == goPidProto
}

func decodeGPid(bytes []byte) (iface.IPid, error) {
	id := binary.BigEndian.Uint32(bytes[1:5])
	hostLength := binary.BigEndian.Uint16(bytes[5:7])
	if len(bytes) < int(hostLength)+7 {
		return nil, error_code.PidFormatError
	}
	host := string(bytes[7:])
	if !IsValidNodeName(host) {
		return nil, error_code.NodeNameFormatError
	}
	return &gPid{
		nodeName: host,
		id:       id,
	}, nil
}

func newPid() iface.IPid {
	return &gPid{
		nodeName: GetNodeName(),
		id:       nextId(),
		//time:     time.Now().Unix(),
	}
}

func DecodePid(bin []byte) (iface.IPid, error) {
	if bin == nil {
		return nil, error_code.PidFormatError
	}
	if isEPid(bin) {
		return decodeEPid(bin)
	} else if isGPid(bin) {
		return decodeGPid(bin)
	}
	return nil, error_code.PidFormatError
}
