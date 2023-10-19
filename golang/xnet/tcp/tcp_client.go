/**
 * @Author: zhangtuo
 * @Description:
 * @File: tcp_client
 * @Data: 2023/7/3 18:34
 */

package tcp

import (
	"golang/iface"
	net2 "golang/xnet"
	"net"
)

func Connect(address string, config *net2.ConnectionConfig) (iface.IConnection, error) {
	conn, err := net.Dial("tcp", address)
	if err != nil {
		return nil, err
	}
	cn := NewConnectionInfo(conn, config)
	return cn, nil
}
