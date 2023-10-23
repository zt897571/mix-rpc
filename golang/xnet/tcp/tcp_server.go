/**
 * @Author: zhangtuo
 * @Description:
 * @File: tcp_server
 * @Data: 2023/7/3 18:36
 */

package tcp

import (
	"context"
	"errors"
	"golang/common/log"
	"golang/iface"
	"golang/xnet"
	"net"
	"sync/atomic"
	"time"
)

type Server struct {
	start      int32
	addr       string
	listen     net.Listener
	connConfig *xnet.ConnectionConfig
	handler    iface.INewConnection
	context    context.Context
	cancel     context.CancelFunc
}

var _ iface.INetServer = (*Server)(nil)

func NewServer(addr string, connConfig *xnet.ConnectionConfig) *Server {
	return &Server{
		connConfig: connConfig,
		addr:       addr,
	}
}

func (s *Server) Start(handler iface.INewConnection, ctx context.Context, startedChannel chan error) {
	var err error
	defer func() {
		if err != nil {
			startedChannel <- err
		}
	}()
	if atomic.LoadInt32(&s.start) == 1 || handler == nil {
		err = errors.New("started")
		return
	}
	s.handler = handler
	ls := net.ListenConfig{
		KeepAlive: 15 * time.Second,
	}
	s.context, s.cancel = context.WithCancel(ctx)
	s.listen, err = ls.Listen(s.context, "tcp", s.addr)
	if err != nil {
		return
	}
	func() {
		defer s.onClose()
		atomic.StoreInt32(&s.start, 1)
		startedChannel <- nil
		for {
			conn, err := s.listen.Accept()
			if err != nil {
				log.Errorf("Accept Error = %s", err)
				return
			}
			ci := NewConnectionInfo(conn, s.connConfig)
			s.handler.OnNewConnection(ci)
		}
	}()
}

func (s *Server) Stop() {
	if atomic.LoadInt32(&s.start) != 1 {
		return
	}
	if s.cancel != nil {
		s.cancel()
	}
	s.onClose()
}

func (s *Server) onClose() {
	atomic.StoreInt32(&s.start, 0)
}
