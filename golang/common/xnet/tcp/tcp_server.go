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
	"net"
	"strconv"
	"sync"
	"sync/atomic"
	"syscall"
)

type Server struct {
	cancelFunc      context.CancelFunc
	start           int32
	listen          net.Listener
	connectionMap   sync.Map
	connctionOption []ConnectionOption
	connConfig      *ConnectionConfig
	port            int
}

func NewServer(port int, connConfig *ConnectionConfig) *Server {
	return &Server{
		connConfig: connConfig,
		port:       port,
	}
}

func (s *Server) Start(option ...ConnectionOption) {
	if atomic.LoadInt32(&s.start) == 1 {
		return
	}
	s.connctionOption = option
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
		defer func() {
			s.close()
		}()
		atomic.StoreInt32(&s.start, 1)
		for {
			conn, err := listen.Accept()
			if err != nil {
				return
			}
			go s.handleConnection(conn)
		}
	}()
}

func (s *Server) handleConnection(conn net.Conn) {
	ci := NewConnectionInfo(conn, s.connConfig, s.connctionOption...)
	ip := ci.GetIp()
	s.connectionMap.Store(ip, ci)
	ci.Run()
	s.connectionMap.Delete(ip)
}

func (s *Server) Stop() {
	if atomic.LoadInt32(&s.start) != 1 {
		return
	}
	s.close()
}

func (s *Server) close() {
	_ = s.listen.Close()
	atomic.StoreInt32(&s.start, 0)
}

func (s *Server) GetConnection() {
}
