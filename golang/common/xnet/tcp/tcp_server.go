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
	iface2 "golang/common/xnet/iface"
	"net"
	"sync/atomic"
	"syscall"
)

type Server struct {
	start      int32
	addr       string
	listen     net.Listener
	connConfig *iface2.ConnectionConfig
	handler    iface2.INewConnection
}

var _ iface2.INetServer = (*Server)(nil)

func NewServer(addr string, connConfig *iface2.ConnectionConfig) *Server {
	return &Server{
		connConfig: connConfig,
		addr:       addr,
	}
}

func (s *Server) Start(handler iface2.INewConnection) error {
	if atomic.LoadInt32(&s.start) == 1 || handler == nil {
		return errors.New("started")
	}
	s.handler = handler
	ls := net.ListenConfig{
		// 重用端口
		Control: func(network, address string, c syscall.RawConn) error {
			return c.Control(func(fd uintptr) {
				err := syscall.SetsockoptInt(syscall.Handle(fd), syscall.SOL_SOCKET, syscall.SO_REUSEADDR, 1)
				if err != nil {
					log.Errorf("SetsockoptInt SO_REUSEADDR error: %v", err)
					return
				}
			})
		},
	}
	listen, err := ls.Listen(context.Background(), "tcp", s.addr)
	if err != nil {
		return err
	}
	s.listen = listen
	func() {
		defer s.onClose()
		atomic.StoreInt32(&s.start, 1)
		for {
			conn, err := listen.Accept()
			if err != nil {
				log.Errorf("Accept Error = %s", err)
				return
			}
			ci := NewConnectionInfo(conn, s.connConfig)
			s.handler.OnNewConnection(ci)
		}
	}()
	return nil
}

func (s *Server) Stop() {
	if atomic.LoadInt32(&s.start) != 1 {
		return
	}
	s.onClose()
}

func (s *Server) onClose() {
	_ = s.listen.Close()
	atomic.StoreInt32(&s.start, 0)
}
