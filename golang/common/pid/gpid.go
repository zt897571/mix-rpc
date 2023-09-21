// Package process -----------------------------
// @file      : gpid.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/24 13:00
// golang pid 代码实现
// format
// [magic num](1), [id](4), [length](2)[hoststring]
// -------------------------------------------
package pid

import (
	"encoding/binary"
	"golang/common/error_code"
	"golang/common/iface"
	"sync/atomic"
)

const localHost = "@"

type gPid struct {
	host string
	id   uint32
}

var _ iface.IPid = (*gPid)(nil)

func (g *gPid) GetHost() string {
	return g.host
}

func (g *gPid) GetId() uint32 {
	return g.id
}

func (g *gPid) IsLocal() bool {
	return g.host == localHost || g.host == iface.GetHost()
}

func (g *gPid) Encode() []byte {
	bin := make([]byte, 7+len(g.host))
	bin[0] = goPidProto
	binary.BigEndian.PutUint32(bin[1:5], g.id)
	binary.BigEndian.PutUint16(bin[5:7], uint16(len(g.host)))
	for i, b := range []byte(g.host) {
		bin[7+i] = b
	}
	return bin
}

var baseIndex uint32

func nextId() uint32 {
	// todo:: zhangtuo 处理达到最大值后重复的情况
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
	return &gPid{
		host: host,
		id:   id,
	}, nil
}

func NewPid() iface.IPid {
	return &gPid{
		host: iface.GetHost(),
		id:   nextId(),
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
