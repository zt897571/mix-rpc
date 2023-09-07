// Package rpc -----------------------------
// @file      : rpc_msg_handler.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/10 11:11
// -------------------------------------------
package rpc

import (
	"encoding/binary"
	"errors"
	"github.com/gogo/protobuf/proto"
	"golang/common/error_code"
	"golang/common/iface"
	"golang/common/log"
	iface2 "golang/common/xnet/iface"
	"golang/common/xnet/tcp"
	xgame "golang/proto"
	"sync"
	"sync/atomic"
	"time"
)

type RpcProxy struct {
	seq        uint32
	conn       iface2.IConnection
	reqWaitMap sync.Map
	remoteHost string
	host       string
}

func NewRpcProxy() iface.IRpcProxy {
	return &RpcProxy{}
}

func (r *RpcProxy) ActorCast(message proto.Message, source iface.IPid, target iface.IPid) error {
	seq := r.NextSeq()
	reqMsg := &xgame.ReqMessage{
		Seq:    seq,
		Source: source.Encode(),
		Target: target.Encode(),
	}
	msg, err := BuildReqMsg(reqMsg, message, BuildFlag([]FlagType{REQ_FLAG}))
	if err != nil {
		return err
	}
	return r.conn.Send(msg)
}

func (r *RpcProxy) OnReceiveMsg(payload []byte) {
	flag := binary.BigEndian.Uint32(payload[:4])
	data := payload[4:]
	if CheckFlag(flag, REQ_FLAG) {
		// 请求消息处理
		if CheckFlag(flag, NODEMSG_FLAG) {
			r.handleNodeReqMsg(data, flag)
		} else {
			r.handleActorReqMsg(data, flag)
		}
	} else {
		// 回复消息处理
		replyMsg := &xgame.ReplyMessage{}
		err := proto.Unmarshal(data, replyMsg)
		if err != nil {
			log.Errorf("Unmarsha error =%v", err)
			return
		}
		if anyChan, ok := r.reqWaitMap.Load(replyMsg.Seq); ok {
			anyChan.(chan *xgame.ReplyMessage) <- replyMsg
		}
	}
}

func (r *RpcProxy) OnDisconnected() {
}

func (r *RpcProxy) SetConnection(conn iface2.IConnection) {
	r.conn = conn
}

func (r *RpcProxy) GetConnection() iface2.IConnection {
	return r.conn
}

func (r *RpcProxy) handleNodeReqMsg(msg []byte, flag uint32) {
	go func() {
		reqMsg, contentMsg, err := UnpackReqMsg(msg)
		if err != nil {
			log.Errorf("Unmarshal error =%v", err)
			return
		}
		handler := GetRpcProxyMgr().GetNodeMsgHandler(reqMsg.MsgName)
		if handler == nil {
			log.Warnf("msg = %s not found Handler", reqMsg.MsgName)
			return
		}
		if CheckFlag(flag, CALL_FLAG) {
			result, err1 := handler.NodeCall(r, contentMsg)
			replyFlag := BuildFlag([]FlagType{NODEMSG_FLAG})
			err = r.ReplyReq(reqMsg, replyFlag, result, err1)
		} else {
			err = handler.NodeCast(r, contentMsg)
		}
		if err != nil {
			log.Errorf("msgName = %s handle Msg Error =%v", reqMsg.MsgName, err)
			return
		}
	}()
}

func (r *RpcProxy) handleNodeResponse(msg []byte) {
	// noderesponse
	replyMsg := &xgame.ReplyMessage{}
	err := proto.Unmarshal(msg, replyMsg)
	if err != nil {
		log.Errorf("Unmarsha error =%v", err)
		return
	}
	if anyChan, ok := r.reqWaitMap.Load(replyMsg.Seq); ok {
		anyChan.(chan *xgame.ReplyMessage) <- replyMsg
	}
}

func (r *RpcProxy) handleActorReqMsg(data []byte, flag uint32) {
	// req msg
	reqMsg := &xgame.ReqMessage{}
	err := proto.Unmarshal(data, reqMsg)
	if err != nil {
		log.Errorf("Unmarsha error =%v", err)
		return
	}
	err = processDispatcher.DispatchMsg(flag, reqMsg, r)
	if err != nil {
		log.Errorf("handle actor msg error =%v", err)
		return
	}
}

// 回复请求
func (r *RpcProxy) ReplyReq(req *xgame.ReqMessage, flag uint32, message proto.Message, err error) error {
	var errorCode string
	if err != nil {
		errorCode = err.Error()
	}
	replyMsg := &xgame.ReplyMessage{
		Seq:     req.Seq,
		ErrCode: errorCode,
	}
	replyBin, err := BuildReplyMsg(replyMsg, message, flag)
	if err != nil {
		return err
	}
	return r.conn.Send(replyBin)
}

func (r *RpcProxy) NextSeq() uint32 {
	return atomic.AddUint32(&r.seq, 1)
}

func (r *RpcProxy) Call(binData []byte, result <-chan *xgame.ReplyMessage) error {
	seq := r.NextSeq()
	r.reqWaitMap.Store(seq, result)
	defer r.reqWaitMap.Delete(seq)
	return r.conn.Send(binData)
}

func (r *RpcProxy) Send(binData []byte) error {
	return r.conn.Send(binData)
}

func (r *RpcProxy) RegSeq(seq uint32, result chan *xgame.ReplyMessage) {
	r.reqWaitMap.Store(seq, result)
}

func (r *RpcProxy) UnRegSeq(seq uint32) {
	r.reqWaitMap.Delete(seq)
}

func (r *RpcProxy) GetHost() string {
	if r.remoteHost == "" {
		r.remoteHost = r.conn.GetRemoteAddress()
	}
	return r.remoteHost
}

var processDispatcher iface.IRpcDispatcher

func RegisterProcessDispatcher(dispatcher iface.IRpcDispatcher) {
	processDispatcher = dispatcher
}

func Connect(IpAddress string) (iface.IRpcProxy, error) {
	connection, err := tcp.Connect(IpAddress, nil)
	if err != nil {
		return nil, err
	}
	rpcProxy := NewRpcProxy()
	connection.BindMsgHandler(rpcProxy)
	go func() {
		mgr := GetRpcProxyMgr()
		mgr.RegisterProxy(rpcProxy)
		defer mgr.RemoveProxy(rpcProxy)
		connection.Run()
	}()
	return rpcProxy, nil
}

func NodeCast(proxy iface.IRpcProxy, message proto.Message) error {
	reqMsg := &xgame.ReqMessage{}
	reqBin, err := BuildReqMsg(reqMsg, message, BuildFlag([]FlagType{NODEMSG_FLAG, REQ_FLAG}))
	if err != nil {
		return err
	}
	return proxy.Send(reqBin)
}

func NodeCall(proxy iface.IRpcProxy, message proto.Message, timeout time.Duration) (proto.Message, error) {
	seq := proxy.NextSeq()
	reqMsg := &xgame.ReqMessage{
		Seq: seq,
	}
	reqBin, err := BuildReqMsg(reqMsg, message, BuildFlag([]FlagType{NODEMSG_FLAG, CALL_FLAG, REQ_FLAG}))
	if err != nil {
		return nil, err
	}
	replyChan := make(chan *xgame.ReplyMessage)
	proxy.RegSeq(seq, replyChan)
	defer proxy.UnRegSeq(seq)
	err = proxy.Send(reqBin)
	if err != nil {
		return nil, err
	}
	select {
	case response := <-replyChan:
		msg, err := GetProtoMsg(response.Payload, response.MsgName)
		if err != nil {
			return nil, err
		}
		if response.ErrCode != "" {
			err = errors.New(response.ErrCode)
		}
		return msg, err
	case <-time.After(timeout):
		return nil, error_code.TimeOutError
	}
}
