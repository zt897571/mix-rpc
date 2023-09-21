// Package utils -----------------------------
// @file      : util.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/24 16:57
// -------------------------------------------
package utils

import (
	"golang/common/error_code"
	"math/rand"
	"reflect"
	"sync"
	"time"
)

type FuncDesc struct {
	InNum    int
	OutNum   int
	Name     string
	InParams []reflect.Type
	OutPrams []reflect.Type
	Index    int
	Method   reflect.Method
	instance reflect.Value
}

func (f *FuncDesc) SafeCall(params ...any) ([]reflect.Value, error) {
	//todo:: recover()
	if len(params) != f.InNum-1 {
		return nil, error_code.FunctionArgsCountError
	}
	pms := make([]reflect.Value, f.InNum)
	pms[0] = f.instance
	for i, p := range params {
		pms[i+1] = reflect.ValueOf(p)
	}
	return f.Method.Func.Call(pms), nil
}

func ScanFunction(st any) []*FuncDesc {
	var fs []*FuncDesc
	tp := reflect.TypeOf(st)
	MethodNum := tp.NumMethod()
	for i := 0; i < MethodNum; i++ {
		method := tp.Method(i)
		methodType := method.Type
		var inParams []reflect.Type
		for ipIdx := 0; ipIdx < methodType.NumIn(); ipIdx++ {
			inParams = append(inParams, methodType.In(ipIdx))
		}
		var outParams []reflect.Type
		for opIdx := 0; opIdx < methodType.NumOut(); opIdx++ {
			outParams = append(inParams, methodType.Out(opIdx))
		}
		fd := &FuncDesc{
			InNum:    methodType.NumIn(),
			OutNum:   methodType.NumOut(),
			Name:     method.Name,
			Index:    i,
			InParams: inParams,
			OutPrams: outParams,
			instance: reflect.ValueOf(st),
			Method:   method,
		}
		fs = append(fs, fd)
	}
	return fs
}

func RandomByte(count int32) []byte {
	data := make([]byte, count)
	rand.Read(data)
	return data
}

func WaitTimeout(wg *sync.WaitGroup, timeout time.Duration) bool {
	c := make(chan struct{})
	go func() {
		defer close(c)
		wg.Wait()
	}()
	select {
	case <-time.After(timeout):
		return true
	case <-c:
		return false
	}
}
