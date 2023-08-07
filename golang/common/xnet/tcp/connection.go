package tcp

import (
	"errors"
	"golang/common/error_code"
	"golang/common/log"
	iface2 "golang/common/xnet/iface"
	"io"
	"net"
	"time"
)

type TcpConnection struct {
	conn          net.Conn
	recvMsgNum    uint32
	activeTime    int64
	readChan      chan []byte
	readChanStop  chan struct{}
	writeChan     chan []byte
	writeChanStop chan struct{}
	config        *iface2.ConnectionConfig
	msgHandler    iface2.INetMsgHandler
}

var _ iface2.IConnection = (*TcpConnection)(nil)

func NewConnectionInfo(conn net.Conn, config *iface2.ConnectionConfig) *TcpConnection {
	if config == nil {
		config = iface2.NewDefaultConnectionConfig()
	}
	ci := &TcpConnection{
		conn:          conn,
		activeTime:    time.Now().Unix(),
		readChan:      make(chan []byte, config.ReadChanelSize),
		readChanStop:  make(chan struct{}),
		writeChan:     make(chan []byte, config.WriteChanelSize),
		writeChanStop: make(chan struct{}),
		config:        config,
	}
	return ci
}

func (ci *TcpConnection) ApplyOption(option ...iface2.ConnectionOption) {
	for _, opt := range option {
		opt(ci.conn)
	}
}

func (ci *TcpConnection) Run() {
	defer ci.OnClose()
	go ci.handleLoop()
	go ci.writeLoop()
	ci.recv()
}

func (ci *TcpConnection) handleLoop() {
	for {
		select {
		case payload := <-ci.readChan:
			ci.handle(payload)
		case <-ci.readChanStop:
			break
		}
	}
}

func (ci *TcpConnection) handle(payload []byte) {
	ci.msgHandler.OnReceiveMsg(payload)
}

func (ci *TcpConnection) writeLoop() {
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

func (ci *TcpConnection) recv() {
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

func (ci *TcpConnection) Send(msg []byte) error {
	// todo:: zhangtuo 处理channel满了的情况
	select {
	case ci.writeChan <- msg:
	default:
		return error_code.ChannelIsFull
	}
	return nil
}

func (ci *TcpConnection) Close() {
	_ = ci.conn.Close()
}

func (ci *TcpConnection) OnClose() {
	ci.msgHandler.OnDisconnected()
}

func (ci *TcpConnection) BindMsgHandler(msgHandler iface2.INetMsgHandler) {
	msgHandler.SetConnection(ci)
	ci.msgHandler = msgHandler
}

func (ci *TcpConnection) GetLocalAddress() string {
	return ci.conn.LocalAddr().String()
}

func (ci *TcpConnection) GetRemoteAddress() string {
	return ci.conn.RemoteAddr().String()
}

func readBytes(conn net.Conn, count int) ([]byte, error) {
	for {
		bytes := make([]byte, count)
		_, err := io.ReadFull(conn, bytes)
		if err != nil {
			if errors.Is(err, io.EOF) {
				log.Infof("Connection Close localAddr = %s remoteAddr = %s", conn.LocalAddr(), conn.RemoteAddr())
			} else if errors.Is(err, net.ErrClosed) {
				log.Infof("remote Close localAddr = %s remoteAddr = %s", conn.LocalAddr(), conn.RemoteAddr())
			} else {
				log.Errorf("connection read error: %v", err)
			}
		}
		if netErr, ok := err.(net.Error); ok && netErr.Timeout() {
			continue
		}
		return bytes, err
	}
}
