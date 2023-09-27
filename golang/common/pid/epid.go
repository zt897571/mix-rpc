// Package pid process -----------------------------
// @file      : epid.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/24 12:07
// -------------------------------------------
package pid

import (
	"encoding/binary"
	"fmt"
	"golang/common/error_code"
	"golang/common/iface"
)

// erlang pid format
// https://www.erlang.org/doc/apps/erts/erl_ext_dist#new_pid_ext

const (
	// proto type
	erlangProto = 131
	goPidProto  = 188

	// etf type
	pidEtfType  = 88
	atomEtfType = 100
)

type ePid struct {
	nodeName iface.NodeName
	id       uint32
	binData  []byte
}

func (p *ePid) String() string {
	return fmt.Sprintf("%s:%d", p.nodeName, p.id)
}

var _ iface.IPid = (*ePid)(nil)

func (p *ePid) GetNodeName() iface.NodeName {
	return p.nodeName
}

func (p *ePid) Encode() []byte {
	return p.binData
}

func (p *ePid) IsLocal() bool {
	return false
}

func isEPid(bytes []byte) bool {
	return len(bytes) > 17 && bytes[0] == erlangProto && bytes[1] == pidEtfType && bytes[2] == atomEtfType
}

// decodeEPid
// [131(proto)],[88(pid_type)], [nodeName], [Id](4), [Serial](4), [Creation](4)
// nodeName -> [100(atom_type), Length(2), "STR"]
func decodeEPid(bin []byte) (iface.IPid, error) {
	nodeLength := binary.BigEndian.Uint16(bin[3:5])
	if len(bin) < int(nodeLength)+17 {
		return nil, error_code.PidFormatError
	}
	nodeName := string(bin[5 : nodeLength+5])
	id := binary.BigEndian.Uint32(bin[nodeLength+5 : nodeLength+9])
	//serial := binary.BigEndian.Uint32(bin[nodeLength+9 : nodeLength+13])
	//creation := binary.BigEndian.Uint32(bin[nodeLength+13 : nodeLength+17])
	if !iface.IsValidNodeName(nodeName) {
		return nil, error_code.NodeNameFormatError
	}
	pid := &ePid{
		nodeName: iface.NodeName(nodeName),
		binData:  bin,
		id:       id,
		//Serial:   serial,
		//Creation: creation,
	}
	return pid, nil
}
