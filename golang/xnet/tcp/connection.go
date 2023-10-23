package tcp

import (
	"context"
	"errors"
	"golang/common/log"
	"golang/iface"
	"golang/xnet"
	"io"
	"net"
	"time"
)

type TcpConnection struct {
	conn       net.Conn
	recvMsgNum uint32
	activeTime int64
	readChan   chan []byte
	writeChan  chan []byte
	config     *xnet.ConnectionConfig
	msgHandler iface.INetMsgHandler
	cancel     context.CancelFunc
	context    context.Context
}

var _ iface.IConnection = (*TcpConnection)(nil)

func NewConnectionInfo(conn net.Conn, config *xnet.ConnectionConfig) *TcpConnection {
	if config == nil {
		config = xnet.NewDefaultConnectionConfig()
	}
	ci := &TcpConnection{
		conn:       conn,
		activeTime: time.Now().Unix(),
		readChan:   make(chan []byte, config.ReadChanelSize),
		writeChan:  make(chan []byte, config.WriteChanelSize),
		config:     config,
	}
	return ci
}

func (ci *TcpConnection) ApplyOption(option ...xnet.ConnectionOption) {
	for _, opt := range option {
		opt(ci.conn)
	}
}

func (ci *TcpConnection) Run(ctx context.Context) {
	ci.context, ci.cancel = context.WithCancel(ctx)
	go ci.handleLoop(ci.context)
	go ci.writeLoop(ci.context)
	ci.recv()
}

func (ci *TcpConnection) handleLoop(ctx context.Context) {
	for {
		select {
		case payload := <-ci.readChan:
			ci.msgHandler.OnReceiveMsg(payload)
		case <-ctx.Done():
			ci.msgHandler.OnDisconnected()
			return
		}
	}
}

func (ci *TcpConnection) writeLoop(ctx context.Context) {
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
		case <-ctx.Done():
			return
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
	select {
	case ci.writeChan <- msg:
		return nil
	}
}

func (ci *TcpConnection) Close() {
	if ci.cancel != nil {
		ci.cancel()
	}
	err := ci.conn.Close()
	if err != nil {
		return
	}
}

func (ci *TcpConnection) BindMsgHandler(msgHandler iface.INetMsgHandler) {
	ci.msgHandler = msgHandler
	msgHandler.SetConnection(ci)
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
