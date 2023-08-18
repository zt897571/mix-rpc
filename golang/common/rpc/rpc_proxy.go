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
	hashCode   iface.HashCode
}

func NewRpcProxy() iface.IRpcProxy {
	return &RpcProxy{}
}

func (r *RpcProxy) NodeCall(message proto.Message, timeout time.Duration) (proto.Message, error) {
	seq := r.nextId()
	reqMsg := &xgame.ReqMessage{
		Seq: seq,
	}
	reqBin, err := BuildReqMsg(reqMsg, message, BuildFlag([]FlagType{NODEMSG_FLAG, CALL_FLAG, REQ_FLAG}))
	if err != nil {
		return nil, err
	}
	return r.sendAndWait(reqBin, seq, timeout)
}

func (r *RpcProxy) NodeCast(message proto.Message) error {
	reqMsg := &xgame.ReqMessage{}
	reqBin, err := BuildReqMsg(reqMsg, message, BuildFlag([]FlagType{NODEMSG_FLAG, REQ_FLAG}))
	if err != nil {
		return err
	}
	return r.conn.Send(reqBin)
}

func (r *RpcProxy) OnReceiveMsg(payload []byte) {
	flag := binary.BigEndian.Uint32(payload[:4])
	data := payload[4:]

	if CheckFlag(flag, NODEMSG_FLAG) {
		if CheckFlag(flag, REQ_FLAG) {
			r.handleNodeReqMsg(data, flag)
		} else {
			r.handleNodeResponse(data)
		}
	} else {
		//actor msg
		if CheckFlag(flag, REQ_FLAG) {
			r.handleActorReqMsg(flag, data)
		} else {
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
			err = r.replyReq(reqMsg, replyFlag, result, err1)
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

func (r *RpcProxy) handleActorReqMsg(flag uint32, data []byte) {
	// req msg
	reqMsg := &xgame.ReqMessage{}
	err := proto.Unmarshal(data, reqMsg)
	if err != nil {
		log.Errorf("Unmarsha error =%v", err)
		return
	}
	// todo:zhangtuo 1. msg <=> msgType
	// 2. 分发到具体process处理
	msg := &xgame.TestMsg{}
	err = proto.Unmarshal(reqMsg.Payload, msg)
	if err != nil {
		return
	}
	if CheckFlag(flag, CALL_FLAG) {
		// reply msg
		//replyBin, _ := proto.Marshal(&xgame.ReplyMessage{
		//	Seq:     reqMsg.Seq,
		//	Payload: reqMsg.Payload,
		//})
		//time.AfterFunc(time.Duration(msg.DelayTime)*time.Second, func() {
		//	flagByte := make([]byte, 4)
		//	ci.config.ByteOrder.PutUint32(flagByte, 0)
		//	ci.Send(append(flagByte, replyBin...))
		//})
	}
}

// 回复请求
func (r *RpcProxy) replyReq(req *xgame.ReqMessage, flag uint32, message proto.Message, err error) error {
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

func (r *RpcProxy) nextId() uint32 {
	return atomic.AddUint32(&r.seq, 1)
}

func (r *RpcProxy) sendMsg(flag uint32, message proto.Message) error {
	data, err := proto.Marshal(message)
	if err != nil {
		return err
	}
	flagByte := make([]byte, 4)
	binary.BigEndian.PutUint32(flagByte, flag)
	return r.conn.Send(append(flagByte, data...))
}

func (r *RpcProxy) sendAndWait(binData []byte, seq uint32, timeout time.Duration) (proto.Message, error) {
	WaitChan := make(chan *xgame.ReplyMessage)
	r.reqWaitMap.Store(seq, WaitChan)
	defer r.reqWaitMap.Delete(seq)
	err := r.conn.Send(binData)
	if err != nil {
		return nil, err
	}
	select {
	case response := <-WaitChan:
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

func (r *RpcProxy) GetHashCode() iface.HashCode {
	if r.hashCode == "" {
		remoteAddr := r.conn.GetRemoteAddress()
		r.hashCode = iface.HashCode(remoteAddr)
	}
	return r.hashCode
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
