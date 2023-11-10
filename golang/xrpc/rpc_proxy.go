// Package xrpc -----------------------------
// @file      : rpc_msg_handler.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/8/10 11:11
// -------------------------------------------
package xrpc

import (
	"context"
	"github.com/gogo/protobuf/proto"
	"golang/common/log"
	"golang/error_code"
	"golang/iface"
	xgame "golang/proto"
	"sync"
	"sync/atomic"
	"time"
)

type iRpcReplyer interface {
	replyReq(seq uint32, message iRpcReplyMsg) error
}

type iRpcReplyMsg interface {
	getRpcResult() (proto.Message, error)
}

type rpcProxy struct {
	seq          uint32
	conn         iface.IConnection
	reqWaitMap   sync.Map
	verified     bool
	nodename     string
	isPassive    bool // 是否为主动连接
	context      context.Context
	cancel       context.CancelFunc
	verifyCancel context.CancelFunc
}

func newRpcProxy(connection iface.IConnection, isPassive bool, nodeName string) *rpcProxy {
	r := &rpcProxy{conn: connection, isPassive: isPassive, nodename: nodeName}
	connection.BindMsgHandler(r)
	return r
}

func (r *rpcProxy) run() {
	r.context, r.cancel = context.WithCancel(context.Background())
	go func() {
		defer func() {
			r.Close()
		}()
		r.conn.Run(r.context)
	}()
	r.tryVerify()
}

func (r *rpcProxy) tryVerify() {
	if r.verified {
		return
	}
	if r.isPassive {
		// 被动验证
		ctx, cancelFunc := context.WithCancel(r.context)
		r.verifyCancel = cancelFunc
		defer cancelFunc()
		select {
		case <-ctx.Done():
			return
		case <-time.After(time.Second * 5):
			r.Close()
			return
		}
	} else {
		// 主动请求验证
		var err error
		var rst proto.Message
		defer func() {
			if err != nil {
				log.Errorf("Verify Reply Error Node = %v", r.nodename)
				r.Close()
			}
		}()
		rst, err = r.blockCall(constVerifyReqFlag, &xgame.ReqVerify{Cookie: GetCookie(), Node: GetNodeName()}, time.Second*5)
		if err != nil {
			return
		}
		if replyMsg, ok := rst.(*xgame.ReplyVerify); !ok || replyMsg.Node != r.nodename {
			err = error_code.VerifyError
			return
		} else {
			r.verified = true
			r.nodename = replyMsg.Node
			gNode.registerProxy(r)
		}
	}
}

func (r *rpcProxy) OnReceiveMsg(payload []byte) {
	pkg, err := newPacket(payload)
	if err != nil {
		log.Errorf("parse packet error = %v", err)
		return
	}
	if pkg.isReq() {
		// 请求消息处理
		if pkg.isNodeMsg() {
			r.handleNodeReqMsg(pkg)
		} else if pkg.isVerifyMsg() {
			r.handleVerifyMsg(pkg)
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

// 处理node消息
func (r *rpcProxy) handleNodeReqMsg(pkg *packet) {
	go func() {
		var err error
		var rst proto.Message
		defer func() {
			if pkg.isCall() {
				if r := recover(); r != nil {
					err = error_code.FunctionPanicError
				}
				msg := buildReplyMsg(rst, err)
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
		if _, ok := gMsgMap[reqMsg.NodeMsg.Module]; !ok {
			err = error_code.ModuleNotFound
			return
		}
		msg, err := getProtoMsg(reqMsg.NodeMsg.Args.Payload, reqMsg.NodeMsg.Args.MsgName)
		if err != nil {
			return
		}
		if pkg.isCall() {
			rst, err = gMsgMap[reqMsg.NodeMsg.Module].HandleCallMsg(reqMsg.NodeMsg.Function, msg)
		} else {
			err = gMsgMap[reqMsg.NodeMsg.Module].HandleCastMsg(reqMsg.NodeMsg.Function, msg)
		}
	}()
}

// 处理actor消息
func (r *rpcProxy) handleProcessReqMsg(pkt *packet) {
	reqMsg := &xgame.ReqMessage{}
	err := proto.Unmarshal(pkt.payload, reqMsg)
	if err != nil {
		log.Errorf("Unmarsha error = %v", err)
		return
	}
	if pkt.isCall() {
		err = gProcessMgr.dispatchCallMsg(pkt.asprocessReqMsg2(), r)
	} else {
		err = gProcessMgr.dispatchCastMsg(pkt.asprocessReqMsg2())
	}
	if err != nil {
		log.Errorf("handle actor msg error = %v", err)
		return
	}
}

// 节点验证消息
func (r *rpcProxy) handleVerifyMsg(pkt *packet) {
	var err error
	defer func() {
		if err != nil {
			log.Errorf("Verify Error = %v", err)
			r.Close()
		}
	}()
	if r.verified {
		err = error_code.VerifyError
		return
	}
	if checkFlag(pkt.flag, REQ_FLAG) {
		reqMsg := &xgame.ReqVerify{}
		err = proto.Unmarshal(pkt.payload, reqMsg)
		if err != nil {
			return
		}
		if reqMsg.Cookie == GetCookie() {
			r.verified = true
			r.nodename = reqMsg.Node
			gNode.registerProxy(r)
			r.verifyCancel()
			err = r.sendMsg(pkt.seq, constVerifyReplyFlag, &xgame.ReplyVerify{Node: GetNodeName()})
			return
		} else {
			err = error_code.VerifyError
			return
		}
	} else {
		replyMsg := &xgame.ReplyVerify{}
		err = proto.Unmarshal(pkt.payload, replyMsg)
		if err != nil && replyMsg.Error != "" {
			return
		}
		r.verified = true
	}
}

func (r *rpcProxy) NextSeq() uint32 {
	return atomic.AddUint32(&r.seq, 1)
}

func (r *rpcProxy) RegSeq(seq uint32, replyBin chan iRpcReplyMsg) {
	r.reqWaitMap.Store(seq, replyBin)
}

func (r *rpcProxy) UnRegSeq(seq uint32) {
	r.reqWaitMap.Delete(seq)
}

func (r *rpcProxy) getRegChan(seq uint32) chan iRpcReplyMsg {
	if anyChan, ok := r.reqWaitMap.Load(seq); ok {
		return anyChan.(chan iRpcReplyMsg)
	}
	return nil
}

func (r *rpcProxy) GetNodeName() string {
	return r.nodename
}

func (r *rpcProxy) replyReq(seq uint32, replyMsg iRpcReplyMsg) error {
	if replyMsg == nil {
		return error_code.ArgumentError
	}
	return r.sendMsg(seq, 0, buildReplyMsg(replyMsg.getRpcResult()))
}

func (r *rpcProxy) OnDisconnected() {
}

func (r *rpcProxy) SetConnection(conn iface.IConnection) {
	r.conn = conn
}

func (r *rpcProxy) GetConnection() iface.IConnection {
	return r.conn
}

func (r *rpcProxy) sendMsg(seq uint32, flag flagType, msg proto.Message) error {
	if r.conn == nil {
		return error_code.NodeNotConnected
	}
	if !r.verified && !checkFlag(flag, VERIFYMSG_FLAG) {
		return error_code.NodeIsNotVerify
	}
	msgBin, err := buildMsg(flag, seq, msg)
	if err != nil {
		return err
	}
	return r.conn.Send(msgBin)
}

func (r *rpcProxy) blockCall(flag flagType, msg proto.Message, timeout time.Duration) (proto.Message, error) {
	channel := newTimeoutChannel[iRpcReplyMsg](1)
	seq := r.NextSeq()
	r.RegSeq(seq, channel.getChannel())
	defer r.UnRegSeq(seq)
	err := r.sendMsg(seq, flag, msg)
	if err != nil {
		return nil, err
	}
	ctx, cancel := context.WithTimeout(r.context, timeout)
	defer cancel()
	rst, err := channel.blockRead(ctx)
	if err != nil {
		return nil, err
	}
	return rst.getRpcResult()
}

func (r *rpcProxy) SendProcessMsg(seq uint32, isCall bool, msg *xgame.ProcessMsg) error {
	flag := constProcessCastFlag
	if isCall {
		flag = constProcessCallFlag
	}
	return r.sendMsg(seq, flag, &xgame.ReqMessage{ProcessMsg: msg})
}

func (r *rpcProxy) Close() {
	if r.cancel != nil {
		r.cancel()
	}
	if r.conn != nil {
		r.conn.Close()
		r.conn = nil
	}
	gNode.removeProxy(r)
}

// todo: zhangtuo function 修改为根据hashid索引
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
