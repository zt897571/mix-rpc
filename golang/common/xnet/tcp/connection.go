package tcp

import (
	"encoding/binary"
	"github.com/gogo/protobuf/proto"
	"golang/common/log"
	"golang/common/xnet/packet"
	xgame "golang/proto"
	"io"
	"net"
	"time"
)

type ConnectionInfo struct {
	conn          net.Conn
	recvMsgNum    uint32
	activeTime    int64
	readChan      chan []byte
	readChanStop  chan struct{}
	writeChan     chan []byte
	writeChanStop chan struct{}
	config        *ConnectionConfig
}

type ConnectionConfig struct {
	ByteOrder       binary.ByteOrder
	PkgHeadLen      int
	ReadChanelSize  int
	WriteChanelSize int
}

func NewDefaultConnectionConfig() *ConnectionConfig {
	return &ConnectionConfig{
		ByteOrder:       binary.BigEndian,
		PkgHeadLen:      4,
		ReadChanelSize:  1000,
		WriteChanelSize: 1000,
	}
}

func NewConnectionInfo(conn net.Conn, config *ConnectionConfig, option ...ConnectionOption) *ConnectionInfo {
	//if tc, ok := conn.(*net.TCPConn); ok {
	//	// todo: zhangtuo config or option
	//	_ = tc.SetNoDelay(true)
	//	_ = tc.SetReadBuffer(1024)
	//	_ = tc.SetWriteBuffer(1024)
	//}
	if config == nil {
		config = NewDefaultConnectionConfig()
	}
	ci := &ConnectionInfo{
		conn:          conn,
		activeTime:    time.Now().Unix(),
		readChan:      make(chan []byte, config.ReadChanelSize),
		readChanStop:  make(chan struct{}),
		writeChan:     make(chan []byte, config.WriteChanelSize),
		writeChanStop: make(chan struct{}),
		config:        config,
	}
	ci.ApplyOption(option...)
	return ci
}

func (ci *ConnectionInfo) ApplyOption(option ...ConnectionOption) {
	for _, opt := range option {
		opt(ci.conn)
	}
}

func (ci *ConnectionInfo) Run() {
	go ci.handleLoop()
	go ci.writeLoop()
	ci.recv()
}

func (ci *ConnectionInfo) handleLoop() {
	for {
		select {
		case payload := <-ci.readChan:
			ci.handle(payload)
		case <-ci.readChanStop:
			break
		}
	}
}

func (ci *ConnectionInfo) handle(payload []byte) {
	flag := ci.config.ByteOrder.Uint32(payload[:4])
	if packet.CheckFlag(flag, packet.REQ_FLAG) {
		// req msg
		reqMsg := &xgame.ReqMessage{}
		err := proto.Unmarshal(payload[4:], reqMsg)
		if err != nil {
			log.Errorf("Unmarsha error =%v", err)
		} else {
			// todo:zhangtuo 1. msg <=> msgType
			// 2. 分发到具体process处理
			msg := &xgame.TestMsg{}
			proto.Unmarshal(reqMsg.Payload, msg)
			log.Info(msg.Msg)
			if packet.CheckFlag(flag, packet.CALL_FLAG) {
				// reply msg
				replyBin, _ := proto.Marshal(&xgame.ReplyMessage{
					Seq:     reqMsg.Seq,
					Payload: reqMsg.Payload,
				})
				time.AfterFunc(time.Duration(msg.DelayTime)*time.Second, func() {
					flagByte := make([]byte, 4)
					ci.config.ByteOrder.PutUint32(flagByte, 0)
					ci.Send(append(flagByte, replyBin...))
				})
			}
		}
	} else {
		//回复消息
	}
}

func (ci *ConnectionInfo) writeLoop() {
	for {
		select {
		case payload := <-ci.writeChan:
			length := len(payload)
			headByte := make([]byte, ci.config.PkgHeadLen)
			ci.config.ByteOrder.PutUint32(headByte, uint32(length))
			_, err := ci.conn.Write(append(headByte, payload...))
			if err != nil {
				if err == io.EOF {
					log.Info("Connection Close")
				} else {
					log.Errorf("connection write error: %v", err)
				}
			}
		case <-ci.writeChanStop:
			break
		}
	}
}

func (ci *ConnectionInfo) recv() {
	defer ci.Close()
	for {
		headByte, err := readBytes(ci.conn, ci.config.PkgHeadLen)
		if err != nil {
			return
		}
		headLen := ci.config.ByteOrder.Uint32(headByte)
		bodyByte, err := readBytes(ci.conn, int(headLen))
		if err != nil {
			return
		}
		ci.recvMsgNum++
		ci.readChan <- bodyByte
	}
}

func (ci *ConnectionInfo) Send(msg []byte) {
	ci.writeChan <- msg
}

func (ci *ConnectionInfo) Close() {
	ci.conn.Close()
}

func (ci *ConnectionInfo) GetIp() string {
	return ci.conn.RemoteAddr().String()
}

func (ci *ConnectionInfo) dispatchMsg(payload []byte) {
}

func readBytes(conn net.Conn, count int) ([]byte, error) {
	for {
		bytes := make([]byte, count)
		_, err := io.ReadFull(conn, bytes)
		if err != nil {
			if err == io.EOF {
				log.Info("Connection Close1")
			} else {
				log.Errorf("connection write error: %v", err)
			}
		}
		if netErr, ok := err.(net.Error); ok && netErr.Timeout() {
			continue
		}
		return bytes, err
	}
}
