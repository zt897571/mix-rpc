/**
 * @Author: zhangtuo
 * @Description:
 * @File: option
 * @Data: 2023/7/4 12:02
 */

package tcp

import "net"

type ConnectionOption func(conn net.Conn)
