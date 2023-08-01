/**
 * @Author: zhangtuo
 * @Description:
 * @File: tcp_client
 * @Data: 2023/7/3 18:34
 */

package tcp

import (
	"golang/common/log"
	"net"
)

type Client struct {
	*ConnectionInfo
}

func NewClient() *Client {
	return &Client{}
}

func (c *Client) Connect(address string) error {
	conn, err := net.Dial("tcp", address)
	if err != nil {
		return err
	}
	c.ConnectionInfo = NewConnectionInfo(conn, c.config)
	c.ConnectionInfo.Run()
	return nil
}

func (c *Client) onDisconnect() {
	log.Info("Connection disconnected")
}
