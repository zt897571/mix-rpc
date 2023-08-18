/**
 * @Author: zhangtuo
 * @Description:
 * @File: tcp_client
 * @Data: 2023/7/3 18:34
 */

package tcp

import (
	iface2 "golang/common/xnet/iface"
	"net"
)

func Connect(address string, config *iface2.ConnectionConfig) (iface2.IConnection, error) {
	conn, err := net.Dial("tcp", address)
	if err != nil {
		return nil, err
	}
	cn := NewConnectionInfo(conn, config)
	return cn, nil
}
