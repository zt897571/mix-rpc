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
	xgame "golang/proto"
	"sync"
	"sync/atomic"
)

type RpcProxy struct {
	seq        uint32
	conn       iface2.IConnection
	reqWaitMap sync.Map
	verified   bool
	nodename   string
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
	if pkg.isVerifyMsg() {
		if r.verified {
			log.Warnf("already verified")
			return
		}
		err = r.handleVerifyMsg(pkg)
		if err != nil {
			r.Close()
			return
		}
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
			log.Warnf("seq = %d not found wait channel", pkg.seq)
			return
		}
		select {
		case rstChan <- pkg:
			return
		default:
			log.Errorf("channel is not in wait status seq = %d", pkg.seq)
		}
	}
}

func (r *RpcProxy) handleNodeReqMsg(pkg *packet) {
	go func() {
		var err error
		var rst proto.Message
		defer func() {
			if pkg.isCall() {
				msg := BuildReplyMsg(rst, err)
				err = r.sendMsg(pkg.seq, 0, msg)
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

func (r *RpcProxy) handleProcessReqMsg(pkt *packet) {
	reqMsg := &xgame.ReqMessage{}
	err := proto.Unmarshal(pkt.payload, reqMsg)
	if err != nil {
		log.Errorf("Unmarsha error = %v", err)
		return
	}
	if pkt.isCall() {
		err = processDispatcher.DispatchCallMsg(pkt.asProcessReqMsg(), r)
	} else {
		err = processDispatcher.DispatchCastMsg(pkt.asProcessReqMsg())
	}
	if err != nil {
		log.Errorf("handle actor msg error = %v", err)
		return
	}
}

func (r *RpcProxy) handleVerifyMsg(pkt *packet) error {
	if CheckFlag(pkt.flag, REQ_FLAG) {
		reqMsg := &xgame.ReqVerify{}
		err := proto.Unmarshal(pkt.payload, reqMsg)
		if err != nil {
			return err
		}
		if reqMsg.Cookie == GetCookie() {
			r.verified = true
			err := r.sendMsg(pkt.seq, 0, &xgame.ReplyVerify{Node: GetNodeName()})
			if err != nil {
				return err
			}
			return nil
		} else {
			return error_code.VerifyError
		}
	} else {
		replyMsg := &xgame.ReplyVerify{}
		err := proto.Unmarshal(pkt.payload, replyMsg)
		if err != nil && replyMsg.Error != "" {
			return err
		}
		r.verified = true
		return nil
	}
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

func (r *RpcProxy) GetNodeName() string {
	return r.nodename
}

func (r *RpcProxy) ReplyReq(seq uint32, replyMsg iface.IRpcReplyMsg) error {
	if replyMsg == nil {
		return error_code.ArgumentError
	}
	return r.sendMsg(seq, 0, BuildReplyMsg(replyMsg.GetRpcResult()))
}

func (r *RpcProxy) OnDisconnected() {
}

func (r *RpcProxy) SetConnection(conn iface2.IConnection) {
	r.conn = conn
}

func (r *RpcProxy) GetConnection() iface2.IConnection {
	return r.conn
}

func (r *RpcProxy) sendMsg(seq uint32, flag FlagType, msg proto.Message) error {
	if r.conn == nil {
		return error_code.RpcNodeNotConnected
	}
	msgBin, err := buildMsg(flag, seq, msg)
	if err != nil {
		return err
	}
	return r.conn.Send(msgBin)
}

func (r *RpcProxy) SendNodeMsg(seq uint32, isCall bool, mfa *xgame.PbMfa) error {
	flag := nodeCastFlag
	if isCall {
		flag = nodeCallFlag
	}
	return r.sendMsg(seq, flag, &xgame.ReqMessage{NodeMsg: mfa})
}

func (r *RpcProxy) SendProcessMsg(seq uint32, isCall bool, msg *xgame.ProcessMsg) error {
	flag := processCastFlag
	if isCall {
		flag = processCallFlag
	}
	return r.sendMsg(seq, flag, &xgame.ReqMessage{ProcessMsg: msg})
}

func (r *RpcProxy) Close() {
	if r.conn != nil {
		r.conn.Close()
		r.conn = nil
	}
}

var processDispatcher iface.IProcessMsgDispatcher
var nodeCallFlag = BuildFlag([]FlagType{NODEMSG_FLAG, REQ_FLAG, CALL_FLAG})
var nodeCastFlag = BuildFlag([]FlagType{NODEMSG_FLAG, REQ_FLAG})

var processCallFlag = BuildFlag([]FlagType{CALL_FLAG, REQ_FLAG})
var processCastFlag = BuildFlag([]FlagType{REQ_FLAG})

func RegisterProcessDispatcher(dispatcher iface.IProcessMsgDispatcher) {
	processDispatcher = dispatcher
}

func BuildMfa(module string, function string, args proto.Message) (*xgame.PbMfa, error) {
	var argsBin []byte = nil
	var err error
	if args != nil {
		argsBin, err = proto.Marshal(args)
		if err != nil {
			return nil, err
		}
	}
	return &xgame.PbMfa{
		Module:   module,
		Function: function,
		Args: &xgame.RpcParams{
			MsgName: proto.MessageName(args),
			Payload: argsBin,
		},
	}, nil
}
