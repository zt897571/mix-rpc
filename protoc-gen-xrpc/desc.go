// Package main -----------------------------
// @file      : desc.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/11/3 15:10
// -------------------------------------------
package main

type ProtoDesc struct {
	FileName       string
	Package        string
	ErlPackageName string
	Services       []*ServiceDesc
}

type ServiceDesc struct {
	Name    string
	ErlName string
	IsActor bool
	Methods []*MethodDesc
}

type MethodDesc struct {
	Name       string
	ErlName    string
	InputType  string
	OutputType string
}

type NameIndex struct {
	GoFuncName  string
	ErlFuncName string
}
