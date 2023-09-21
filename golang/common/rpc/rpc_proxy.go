// Package rpc -----------------------------
// @file      : rpc_msg_handler.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/10 11:11
// -------------------------------------------
package rpc

import (
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
	localHost  string
}

var _ iface.IRpcProxy = (*RpcProxy)(nil)

func NewRpcProxy() iface.IRpcProxy {
	return &RpcProxy{}
}

func (r *RpcProxy) OnReceiveMsg(payload []byte) {
	pkg, err := newPacket(payload)
	if err != nil {
		log.Errorf("parse packet error = %v", err)
		return
	}
	if pkg.isReq() {
		// 请求消息处理
		if pkg.isNodeMsg() {
			r.handleNodeReqMsg(pkg)
		} else {
			r.handleProcessReqMsg(pkg)
		}
	} else {
		// 回复消息处理
		rstChan := r.getRegChan(pkg.seq)
		if rstChan == nil {
			log.Warnf("seq = %s not found wait channel", pkg.seq)
			return
		}
		select {
		case rstChan <- pkg:
			return
		default:
			log.Errorf("channel is not in wait status seq = %s", pkg.seq)
		}
	}
}

func (r *RpcProxy) handleNodeReqMsg(pkg *packet) {
	go func() {
		var err error
		var rst proto.Message
		defer func() {
			if pkg.isCall() {
				replyFlag := BuildFlag([]iface.FlagType{NODEMSG_FLAG})
				msg := &xgame.ReplyMessage{Result: BuildRpcResult(rst, err)}
				err = r.SendMsg(pkg.seq, replyFlag, msg)
				if err != nil {
					log.Errorf("Reply Request error = %v", err)
				}
			}
		}()
		reqMsg := &xgame.ReqMessage{}
		err = proto.Unmarshal(pkg.payload, reqMsg)
		if err != nil {
			return
		}
		if reqMsg.NodeMsg == nil {
			err = error_code.MfaError
			return
		}
		// todo:: zhangtuo recover
		rst, err = applyMfa(reqMsg.NodeMsg)
	}()
}

func (r *RpcProxy) handleNodeResponse(msg []byte, seq uint32) {
	replyMsg := &xgame.ReplyMessage{}
	err := proto.Unmarshal(msg, replyMsg)
	if err != nil {
		log.Errorf("Unmarsha error =%v", err)
		return
	}
	if anyChan, ok := r.reqWaitMap.Load(seq); ok {
		anyChan.(chan *xgame.ReplyMessage) <- replyMsg
	}
}

func (r *RpcProxy) handleProcessReqMsg(pkg *packet) {
	// req msg
	reqMsg := &xgame.ReqMessage{}
	err := proto.Unmarshal(pkg.payload, reqMsg)
	if err != nil {
		log.Errorf("Unmarsha error =%v", err)
		return
	}
	err = processDispatcher.DispatchMsg(pkg.asProcessReqMsg(), r)
	if err != nil {
		log.Errorf("handle actor msg error =%v", err)
		return
	}
}

func (r *RpcProxy) SendMsg(seq uint32, flag iface.FlagType, msg proto.Message) error {
	msgBin, err := buildMsg(flag, seq, msg)
	if err != nil {
		return err
	}
	return r.conn.Send(msgBin)
}

func (r *RpcProxy) NextSeq() uint32 {
	return atomic.AddUint32(&r.seq, 1)
}

func (r *RpcProxy) RegSeq(seq uint32, replyBin chan iface.IRpcReplyMsg) {
	r.reqWaitMap.Store(seq, replyBin)
}

func (r *RpcProxy) UnRegSeq(seq uint32) {
	r.reqWaitMap.Delete(seq)
}

func (r *RpcProxy) getRegChan(seq uint32) chan iface.IRpcReplyMsg {
	if anyChan, ok := r.reqWaitMap.Load(seq); ok {
		return anyChan.(chan iface.IRpcReplyMsg)
	}
	return nil
}

func (r *RpcProxy) GetRemoteHost() string {
	if r.remoteHost == "" {
		r.remoteHost = r.conn.GetRemoteAddress()
	}
	return r.remoteHost
}

func (r *RpcProxy) GetLocalHost() string {
	if r.localHost == "" {
		r.localHost = r.conn.GetLocalAddress()
	}
	return r.localHost
}

func (r *RpcProxy) ReplyReq(seq uint32, replyMsg iface.IRpcReplyMsg) error {
	if replyMsg == nil {
		return error_code.ArgumentError
	}
	msg := &xgame.ReplyMessage{
		Result: BuildRpcResult(replyMsg.GetRpcResult()),
	}
	return r.SendMsg(seq, 0, msg)
}

func (r *RpcProxy) OnDisconnected() {
}

func (r *RpcProxy) SetConnection(conn iface2.IConnection) {
	r.conn = conn
}

func (r *RpcProxy) GetConnection() iface2.IConnection {
	return r.conn
}

var processDispatcher iface.IProcessMsgDispatcher
var nodeCallFlag = BuildFlag([]iface.FlagType{NODEMSG_FLAG, REQ_FLAG, CALL_FLAG})
var nodeCastFlag = BuildFlag([]iface.FlagType{NODEMSG_FLAG, REQ_FLAG})

func RegisterProcessDispatcher(dispatcher iface.IProcessMsgDispatcher) {
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

func NodeCast(proxy iface.IRpcProxy, mfa *xgame.Mfa) error {
	reqMsg := &xgame.ReqMessage{
		NodeMsg: mfa,
	}
	seq := proxy.NextSeq()
	return proxy.SendMsg(seq, nodeCastFlag, reqMsg)
}

func NodeCall(proxy iface.IRpcProxy, mfa *xgame.Mfa, timeout time.Duration) (proto.Message, error) {
	seq := proxy.NextSeq()
	reqMsg := &xgame.ReqMessage{
		NodeMsg: mfa,
	}
	replyChan := make(chan iface.IRpcReplyMsg, 1)
	proxy.RegSeq(seq, replyChan)
	defer proxy.UnRegSeq(seq)
	err := proxy.SendMsg(seq, nodeCallFlag, reqMsg)
	if err != nil {
		return nil, err
	}
	select {
	case replyMsg := <-replyChan:
		return replyMsg.GetRpcResult()
	case <-time.After(timeout):
		return nil, error_code.TimeOutError
	}
}

func BuildMfa(module string, function string, args proto.Message) (*xgame.Mfa, error) {
	var argsBin []byte = nil
	var err error
	if args != nil {
		argsBin, err = proto.Marshal(args)
		if err != nil {
			return nil, err
		}
	}
	return &xgame.Mfa{
		Module:   module,
		Function: function,
		Args: &xgame.RpcParams{
			MsgName: proto.MessageName(args),
			Payload: argsBin,
		},
	}, nil
}
