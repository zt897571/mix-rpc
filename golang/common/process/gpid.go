// Package process -----------------------------
// @file      : gpid.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/24 13:00
// golang pid 代码实现
// format
// [magic num](1), [id](4), [length](2)[hoststring]
// -------------------------------------------
package process

import (
	"encoding/binary"
	"golang/common/error_code"
	"golang/common/iface"
	"sync/atomic"
)

func init() {
	fc := &GPidFactory{}
	RegisterPidFactory(fc)
	RegisterPidDecoder(fc)
}

type GPid struct {
	host string
	id   uint32
}

var _ iface.IPid = (*GPid)(nil)

func (g *GPid) GetHost() string {
	return g.host
}

func (g *GPid) GetId() uint32 {
	return g.id
}

func (g *GPid) Encode() []byte {
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

type GPidFactory struct {
}

var _ iface.IPidFactory = (*GPidFactory)(nil)

func (g *GPidFactory) NewPid() iface.IPid {
	return &GPid{
		host: iface.GetHost(),
		id:   g.nextId(),
	}
}

func (g *GPidFactory) nextId() uint32 {
	// todo:: zhangtuo 处理达到最大值后重复的情况
	return atomic.AddUint32(&baseIndex, 1)
}

func (g *GPidFactory) Check(bytes []byte) bool {
	return len(bytes) >= 7 && bytes[0] == goPidProto
}

func (g *GPidFactory) DecodePid(bytes []byte) (iface.IPid, error) {
	id := binary.BigEndian.Uint32(bytes[1:5])
	hostLength := binary.BigEndian.Uint16(bytes[5:7])
	if len(bytes) < int(hostLength)+7 {
		return nil, error_code.PidFormatError
	}
	host := string(bytes[7:])
	return &GPid{
		host: host,
		id:   id,
	}, nil
}
