// Package process -----------------------------
// @file      : pid.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/22 19:09
// -------------------------------------------

package process

import (
	"golang/common/error_code"
	"golang/common/iface"
)

var newPidImpl iface.IPidFactory
var decodePidImpls []iface.IPidDecoder

func RegisterPidFactory(i iface.IPidFactory) {
	newPidImpl = i
}

func RegisterPidDecoder(i iface.IPidDecoder) {
	decodePidImpls = append(decodePidImpls, i)
}

func NewPid() iface.IPid {
	return newPidImpl.NewPid()
}

func DecodePid(bin []byte) (iface.IPid, error) {
	for _, de := range decodePidImpls {
		if de.Check(bin) {
			return de.DecodePid(bin)
		}
	}
	return nil, error_code.PidDecoderNotFound
}
