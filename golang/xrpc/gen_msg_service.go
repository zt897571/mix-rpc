// Code generated by protoc-gen-go-xrpc. DO NOT EDIT.
// source file: msg.proto
// template: xrpc-go.tmpl

package xrpc

import (
	"github.com/gogo/protobuf/proto"
	"golang/common/log"
	"golang/error_code"
	"golang/iface"
	xgame "golang/proto"
	"time"
)

type ITestService interface {
	CallTest(iface.IPid, *xgame.TestMsg, time.Duration) (*xgame.TestMsg, error)
	CallGetPidList(iface.IPid, *xgame.ReqGetPidList, time.Duration) (*xgame.ReplyGetPidList, error)
}

type ITestServiceHandler interface {
	OnCallTest(iface.IPid, *xgame.TestMsg) (*xgame.TestMsg, error)
	OnCastTest(iface.IPid, *xgame.TestMsg) error
	OnCallGetPidList(iface.IPid, *xgame.ReqGetPidList) (*xgame.ReplyGetPidList, error)
	OnCastGetPidList(iface.IPid, *xgame.ReqGetPidList) error
}

type TestService struct {
	iface.IProcess
}

func NewTestService(process iface.IProcess) *TestService {
	return &TestService{IProcess: process}
}

func (s *TestService) CallTest(pid iface.IPid, req *xgame.TestMsg, timeout time.Duration) (*xgame.TestMsg, error) {
	reply, err := s.Call(pid, req, timeout)
	if err != nil {
		return nil, err
	}
	if msg, ok := reply.(*xgame.TestMsg); ok {
		return msg, nil
	} else {
		return nil, error_code.MsgTypeError
	}
}

func (s *TestService) CallGetPidList(pid iface.IPid, req *xgame.ReqGetPidList, timeout time.Duration) (*xgame.ReplyGetPidList, error) {
	reply, err := s.Call(pid, req, timeout)
	if err != nil {
		return nil, err
	}
	if msg, ok := reply.(*xgame.ReplyGetPidList); ok {
		return msg, nil
	} else {
		return nil, error_code.MsgTypeError
	}
}

func CallTest(pid iface.IPid, req *xgame.TestMsg, timeout time.Duration) (*xgame.TestMsg, error) {
	reply, err := Call(pid, req, timeout)
	if err != nil {
		return nil, err
	}
	if msg, ok := reply.(*xgame.TestMsg); ok {
		return msg, nil
	} else {
		return nil, error_code.MsgTypeError
	}
}

func CallGetPidList(pid iface.IPid, req *xgame.ReqGetPidList, timeout time.Duration) (*xgame.ReplyGetPidList, error) {
	reply, err := Call(pid, req, timeout)
	if err != nil {
		return nil, err
	}
	if msg, ok := reply.(*xgame.ReplyGetPidList); ok {
		return msg, nil
	} else {
		return nil, error_code.MsgTypeError
	}
}

type ITestNodeServiceHandler interface {
	GetModuleName() string
	OnCallNodeTest(*xgame.TestMsg) (*xgame.TestMsg, error)
	OnCastNodeTest(*xgame.TestMsg) error
	OnCallNodeGetPidList(*xgame.ReqGetPidList) (*xgame.ReplyGetPidList, error)
	OnCastNodeGetPidList(*xgame.ReqGetPidList) error
}

func CallNodeTest(node string, module string, req *xgame.TestMsg, timeout time.Duration) (*xgame.TestMsg, error) {
	mfa, err := BuildMfa(module, "OnCallNodeTest", req)
	if err != nil {
		return nil, err
	}
	reply, err := NodeCall(node, mfa, timeout)
	if err != nil {
		return nil, err
	}
	if msg, ok := reply.(*xgame.TestMsg); ok {
		return msg, nil
	} else {
		return nil, error_code.MsgTypeError
	}
}

func CastNodeTest(node string, module string, req *xgame.TestMsg) error {
	mfa, err := BuildMfa(module, "OnCastNodeTest", req)
	if err != nil {
		return err
	}
	return NodeCast(node, mfa)
}

func CallNodeGetPidList(node string, module string, req *xgame.ReqGetPidList, timeout time.Duration) (*xgame.ReplyGetPidList, error) {
	mfa, err := BuildMfa(module, "OnCallNodeGetPidList", req)
	if err != nil {
		return nil, err
	}
	reply, err := NodeCall(node, mfa, timeout)
	if err != nil {
		return nil, err
	}
	if msg, ok := reply.(*xgame.ReplyGetPidList); ok {
		return msg, nil
	} else {
		return nil, error_code.MsgTypeError
	}
}

func CastNodeGetPidList(node string, module string, req *xgame.ReqGetPidList) error {
	mfa, err := BuildMfa(module, "OnCastNodeGetPidList", req)
	if err != nil {
		return err
	}
	return NodeCast(node, mfa)
}

func DispatchTestNodeServiceCallMsg(handler ITestNodeServiceHandler, msgName string, msg proto.Message) (proto.Message, error) {
	switch msgName {
	case "OnCallNodeTest":
		return handler.OnCallNodeTest(msg.(*xgame.TestMsg))
	case "OnCallNodeGetPidList":
		return handler.OnCallNodeGetPidList(msg.(*xgame.ReqGetPidList))
	}
	log.Errorf("DispatchTestNodeServiceCallMsg not found msgName:%s", msgName)
	return nil, error_code.MsgHandlerNotFound
}

func DispatchTestNodeServiceCastMsg(handler ITestNodeServiceHandler, msgName string, msg proto.Message) error {
	switch msgName {
	case "OnCastNodeTest":
		return handler.OnCastNodeTest(msg.(*xgame.TestMsg))
	case "OnCastNodeGetPidList":
		return handler.OnCastNodeGetPidList(msg.(*xgame.ReqGetPidList))
	}
	log.Errorf("DispatchTestNodeServiceCallMsg not found msgName:%s", msgName)
	return error_code.MsgHandlerNotFound
}