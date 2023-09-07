// Package utils -----------------------------
// @file      : util.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/24 16:57
// -------------------------------------------
package utils

import (
	"reflect"
)

type FuncDesc struct {
	inNum    int
	outNum   int
	name     string
	inParams []reflect.Type
	outPrams []reflect.Type
	index    int
	method   reflect.Value
}

type StructDesc struct {
	methods []*FuncDesc
}

func ScanStruct(st any) *StructDesc {
	rs := &StructDesc{}
	tp := reflect.TypeOf(st)
	MethodNum := tp.NumMethod()
	for i := 0; i < MethodNum; i++ {
		method := tp.Method(i).Type
		var inParams []reflect.Type
		for ipIdx := 0; ipIdx < method.NumIn(); ipIdx++ {
			inParams = append(inParams, method.In(ipIdx))
		}
		var outParams []reflect.Type
		for opIdx := 0; opIdx < method.NumOut(); opIdx++ {
			outParams = append(inParams, method.Out(opIdx))
		}
		fd := &FuncDesc{
			inNum:    method.NumIn(),
			outNum:   method.NumOut(),
			name:     method.Name(),
			index:    i,
			inParams: inParams,
			outPrams: outParams,
		}
		rs.methods = append(rs.methods, fd)
	}
	return nil
}
