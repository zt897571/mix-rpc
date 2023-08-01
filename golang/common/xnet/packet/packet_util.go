// Package packet -----------------------------
// @file      : packet_util.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/7/31 9:55
// -------------------------------------------
package packet

type FlagType uint32

const (
	REQ_FLAG            FlagType = 0b1
	CALL_FLAG                    = 0b10
	NODEMSG_FLAG                 = 0b100
	IS_ERLANG_NODE_FLAG          = 0b1000
)

func BuildFlag(flagList []FlagType) uint32 {
	var result uint32
	for _, f := range flagList {
		result = uint32(f) ^ result
	}
	return result
}

func CheckFlag(flag uint32, flagType FlagType) bool {
	flagTypeUint32 := uint32(flagType)
	return flag&flagTypeUint32 == flagTypeUint32
}
