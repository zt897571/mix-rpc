package main

import (
	"bufio"
	"flag"
	"fmt"
	"golang/common/error_code"
	iface2 "golang/common/iface"
	"golang/common/log"
	"golang/common/rpc"
	xgame "golang/proto"
	"os"
	"strings"
	"time"
)

var gHost = flag.String("host", "10.2.58.210:8000", "host")

var gCmd2Func = map[string]func(args []string) error{
	"connect":   connect,
	"nodecall":  nodecall,
	"nodecast":  nodecast,
	"listProxy": listProxy,
}

func main() {
	addrSp := strings.Split(*gHost, ":")
	iface2.SetHost(addrSp[0])
	server := rpc.NewRpcServer(*gHost)
	err := server.Start(true)
	if err != nil {
		log.Errorf("start server error = %s", err)
		return
	}
	rpc.RegisterNodeMsg("test", &NodeCmd{})

	reader := bufio.NewReader(os.Stdin)
	for {
		fmt.Print("-> ")
		rawText, _ := reader.ReadString('\n')
		text := strings.Trim(strings.Trim(rawText, "\r\n"), "\n")
		cmds := strings.Split(text, " ")
		if f, ok := gCmd2Func[cmds[0]]; ok {
			err := f(cmds[1:])
			if err != nil {
				fmt.Println(err)
				return
			} else {
				fmt.Println("ok")
			}
		} else {
			fmt.Println("unknow command")
		}
	}
}

var rpcProxy iface2.IRpcProxy

func connect(args []string) error {
	targetHost := args[0]
	proxy, err := rpc.Connect(targetHost)
	if err != nil {
		return err
	}
	rpcProxy = proxy
	return nil
}

func nodecall(args []string) error {
	if rpcProxy == nil {
		return error_code.RpcHostNotConnected
	}
	if len(args) < 1 {
		return error_code.ArgumentError
	}
	mfa, err := rpc.BuildMfa("test", "TestNodeCall", &xgame.TestMsg{Msg: args[0]})
	if err != nil {
		return err
	}
	rst, err := rpc.NodeCall(rpcProxy, mfa, time.Second)
	if err != nil {
		return err
	}
	fmt.Printf("%v\n", rst)
	return nil
}

func nodecast(args []string) error {
	if rpcProxy == nil {
		return error_code.RpcHostNotConnected
	}
	if len(args) < 1 {
		return error_code.ArgumentError
	}
	mfa, err := rpc.BuildMfa("test", "TestNodeCast", &xgame.TestMsg{Msg: args[0]})
	if err != nil {
		return err
	}
	return rpc.NodeCast(rpcProxy, mfa)
}

func listProxy(_ []string) error {
	proxyMgr := rpc.GetRpcProxyMgr()
	proxyList := proxyMgr.GetAllProxy()
	for _, proxy := range proxyList {
		fmt.Printf("RpcProxy: local = %s, remote = %s", proxy.GetLocalHost(), proxy.GetRemoteHost())
	}
	return nil
}
