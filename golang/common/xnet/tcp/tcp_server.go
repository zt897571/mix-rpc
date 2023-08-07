/**
 * @Author: zhangtuo
 * @Description:
 * @File: tcp_server
 * @Data: 2023/7/3 18:36
 */

package tcp

import (
	"context"
	"golang/common/log"
	iface2 "golang/common/xnet/iface"
	"net"
	"strconv"
	"sync/atomic"
	"syscall"
)

type Server struct {
	start           int32
	listen          net.Listener
	connctionOption []iface2.ConnectionOption
	connConfig      *iface2.ConnectionConfig
	port            int
	handler         iface2.INewConnection
}

var _ iface2.IServer = (*Server)(nil)

func NewServer(port int, connConfig *iface2.ConnectionConfig) *Server {
	return &Server{
		connConfig: connConfig,
		port:       port,
	}
}

func (s *Server) Start(handler iface2.INewConnection) {
	if atomic.LoadInt32(&s.start) == 1 || handler == nil {
		return
	}
	s.handler = handler
	hostPort := net.JoinHostPort("", strconv.Itoa(s.port))
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
	listen, err := ls.Listen(context.Background(), "tcp", hostPort)
	if err != nil {
		log.Errorf("Listen error: %v", err)
		return
	}
	s.listen = listen
	func() {
		defer s.onClose()
		atomic.StoreInt32(&s.start, 1)
		for {
			conn, err := listen.Accept()
			if err != nil {
				return
			}
			s.handleConnection(conn)
		}
	}()
}

func (s *Server) SetConnectionOpt(option ...iface2.ConnectionOption) {
	s.connctionOption = option
}

func (s *Server) handleConnection(conn net.Conn) {
	ci := NewConnectionInfo(conn, s.connConfig)
	s.handler.OnNewConnection(ci)
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
