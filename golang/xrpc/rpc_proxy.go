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

type IRpcReplyer interface {
	ReplyReq(seq uint32, message IRpcReplyMsg) error
}

type IRpcReplyMsg interface {
	GetRpcResult() (proto.Message, error)
}

type RpcProxy struct {
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

func newRpcProxy(connection iface.IConnection, isPassive bool, nodeName string) *RpcProxy {
	r := &RpcProxy{conn: connection, isPassive: isPassive, nodename: nodeName}
	connection.BindMsgHandler(r)
	return r
}

func (r *RpcProxy) run() {
	r.context, r.cancel = context.WithCancel(context.Background())
	go func() {
		defer gNode.removeProxy(r)
		r.conn.Run(r.context)
		log.Infof("rpr proxy Stop = %v", r.nodename)
	}()
	r.tryVerify()
}

func (r *RpcProxy) tryVerify() {
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
				r.Close()
			}
		}()
		rst, err = r.blockCall(constVerifyReqFlag, &xgame.ReqVerify{Cookie: GetCookie(), Node: GetNodeName()}, time.Second*5)
		if err != nil {
			log.Errorf("Req Verify Error Node = %v err= %v", r.nodename, err)
			r.Close()
			return
		}
		if replyMsg, ok := rst.(*xgame.ReplyVerify); !ok || replyMsg.Node != r.nodename {
			log.Errorf("Verify Reply Error Node = %v", r.nodename)
			r.Close()
			return
		} else {
			r.verified = true
			r.nodename = replyMsg.Node
			gNode.registerProxy(r)
		}
	}
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

// 处理actor消息
func (r *RpcProxy) handleProcessReqMsg(pkt *packet) {
	reqMsg := &xgame.ReqMessage{}
	err := proto.Unmarshal(pkt.payload, reqMsg)
	if err != nil {
		log.Errorf("Unmarsha error = %v", err)
		return
	}
	if pkt.isCall() {
		err = gProcessMgr.DispatchCallMsg(pkt.asprocessReqMsg2(), r)
	} else {
		err = gProcessMgr.DispatchCastMsg(pkt.asprocessReqMsg2())
	}
	if err != nil {
		log.Errorf("handle actor msg error = %v", err)
		return
	}
}

// 节点验证消息
func (r *RpcProxy) handleVerifyMsg(pkt *packet) {
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
	if CheckFlag(pkt.flag, REQ_FLAG) {
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

func (r *RpcProxy) NextSeq() uint32 {
	return atomic.AddUint32(&r.seq, 1)
}

func (r *RpcProxy) RegSeq(seq uint32, replyBin chan IRpcReplyMsg) {
	r.reqWaitMap.Store(seq, replyBin)
}

func (r *RpcProxy) UnRegSeq(seq uint32) {
	r.reqWaitMap.Delete(seq)
}

func (r *RpcProxy) getRegChan(seq uint32) chan IRpcReplyMsg {
	if anyChan, ok := r.reqWaitMap.Load(seq); ok {
		return anyChan.(chan IRpcReplyMsg)
	}
	return nil
}

func (r *RpcProxy) GetNodeName() string {
	return r.nodename
}

func (r *RpcProxy) ReplyReq(seq uint32, replyMsg IRpcReplyMsg) error {
	if replyMsg == nil {
		return error_code.ArgumentError
	}
	return r.sendMsg(seq, 0, BuildReplyMsg(replyMsg.GetRpcResult()))
}

func (r *RpcProxy) OnDisconnected() {
}

func (r *RpcProxy) SetConnection(conn iface.IConnection) {
	r.conn = conn
}

func (r *RpcProxy) GetConnection() iface.IConnection {
	return r.conn
}

func (r *RpcProxy) sendMsg(seq uint32, flag FlagType, msg proto.Message) error {
	if r.conn == nil {
		return error_code.NodeNotConnected
	}
	if !r.verified && !CheckFlag(flag, VERIFYMSG_FLAG) {
		return error_code.NodeIsNotVerify
	}
	msgBin, err := buildMsg(flag, seq, msg)
	if err != nil {
		return err
	}
	return r.conn.Send(msgBin)
}

func (r *RpcProxy) blockCall(flag FlagType, msg proto.Message, timeout time.Duration) (proto.Message, error) {
	channel := newTimeoutChannel[IRpcReplyMsg](1)
	seq := r.NextSeq()
	r.RegSeq(seq, channel.getChannel())
	defer r.UnRegSeq(seq)
	err := r.sendMsg(seq, flag, msg)
	if err != nil {
		return nil, err
	}
	rst, err := channel.blockRead(timeout)
	if err != nil {
		return nil, err
	}
	return rst.GetRpcResult()
}

func (r *RpcProxy) SendProcessMsg(seq uint32, isCall bool, msg *xgame.ProcessMsg) error {
	flag := constProcessCastFlag
	if isCall {
		flag = constProcessCallFlag
	}
	return r.sendMsg(seq, flag, &xgame.ReqMessage{ProcessMsg: msg})
}

func (r *RpcProxy) Close() {
	if r.cancel != nil {
		r.cancel()
	}
	if r.conn != nil {
		r.conn.Close()
		r.conn = nil
	}
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
