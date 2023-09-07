// Package process -----------------------------
// @file      : erlang_pid.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/24 12:07
// -------------------------------------------
package process

import (
	"encoding/binary"
	"golang/common/error_code"
	"golang/common/iface"
	"strings"
)

// erlang pid format
// https://www.erlang.org/doc/apps/erts/erl_ext_dist#new_pid_ext

func init() {
	RegisterPidDecoder(&EPidFactory{})
}

const (
	// proto type
	erlangProto = 131
	goPidProto  = 188

	// etf type
	pidEtfType  = 88
	atomEtfType = 100
)

type EPid struct {
	Host    string
	ID      uint32
	binData []byte
}

var _ iface.IPid = (*EPid)(nil)

func (p *EPid) GetHost() string {
	return p.Host
}

func (p *EPid) GetId() uint32 {
	return p.ID
}

func (p *EPid) Encode() []byte {
	return p.binData
}

type EPidFactory struct {
}

var _ iface.IPidDecoder = (*EPidFactory)(nil)

func (p *EPidFactory) Check(bytes []byte) bool {
	return len(bytes) > 17 && bytes[0] == erlangProto && bytes[1] == pidEtfType && bytes[2] == atomEtfType
}

// DecodePid
// [131(proto)],[88(pid_type)], [Host], [Id](4), [Serial](4), [Creation](4)
// Host -> [100(atom_type), Length(2), "STR"]
func (p *EPidFactory) DecodePid(bin []byte) (iface.IPid, error) {
	nodeLength := binary.BigEndian.Uint16(bin[3:5])
	if len(bin) < int(nodeLength)+17 {
		return nil, error_code.PidFormatError
	}
	node := string(bin[5 : nodeLength+5])
	id := binary.BigEndian.Uint32(bin[nodeLength+5 : nodeLength+9])
	//serial := binary.BigEndian.Uint32(bin[nodeLength+9 : nodeLength+13])
	//creation := binary.BigEndian.Uint32(bin[nodeLength+13 : nodeLength+17])
	pid := &EPid{
		Host:    strings.Split(node, "@")[1],
		binData: bin,
		ID:      id,
		//Serial:   serial,
		//Creation: creation,
	}
	return pid, nil
}
