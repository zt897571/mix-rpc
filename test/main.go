package main

import (
	"bufio"
	"flag"
	"fmt"
	"golang/common/error_code"
	iface2 "golang/common/iface"
	"golang/common/log"
	"golang/common/pid"
	"golang/common/process"
	"golang/common/rpc"
	xgame "golang/proto"
	"os"
	"strings"
	"time"
)

var gHost = flag.String("host", "10.2.58.210:8000", "host")
var nodeName = flag.String("nodeName", "noname", "nodeName")

var gCmd2Func = map[string]func(args []string) error{
	"connect":          connect,
	"nodecall":         nodecall,
	"nodecast":         nodecast,
	"listProxy":        listProxy,
	"getRemotePidList": getRemotePidList,
	"actorCall":        actorCall,
	"actorCast":        actorCast,
	"createProcess":    createProcess,
}

func main() {
	flag.Parse()
	addrSp := strings.Split(*gHost, ":")
	// todo: zhangtuo 重构整合到 node
	err := iface2.SetNodeName(fmt.Sprintf("%s@%s", *nodeName, addrSp[0]))
	if err != nil {
		log.Errorf("set node name error = %s", err)
		return
	}
	server := rpc.NewRpcServer(*gHost)
	err = server.Start()
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
		if text == "" {
			continue
		}
		cmds := strings.Split(text, " ")
		if f, ok := gCmd2Func[cmds[0]]; ok {
			err := safeExec(f, cmds[1:])
			if err != nil {
				fmt.Println(err)
			} else {
				fmt.Println("ok")
			}
		} else {
			fmt.Println("unknow command")
		}
	}
}

var remotePidList []iface2.IPid

func getRpcProxy() iface2.IRpcProxy {
	allProxy := rpc.GetRpcProxyMgr().GetAllProxy()
	if len(allProxy) > 0 {
		return allProxy[0]
	} else {
		return nil
	}
}

func connect(args []string) error {
	targetHost := args[0]
	_, err := rpc.Connect(targetHost)
	if err != nil {
		return err
	}
	return nil
}

func nodecall(args []string) error {
	if getRpcProxy() == nil {
		return error_code.RpcHostNotConnected
	}
	if len(args) < 1 {
		return error_code.ArgumentError
	}
	mfa, err := rpc.BuildMfa("test", "TestNodeCall", &xgame.TestMsg{Msg: args[0]})
	if err != nil {
		return err
	}
	rst, err := rpc.NodeCall(getRpcProxy(), mfa, time.Second)
	if err != nil {
		return err
	}
	fmt.Printf("%v\n", rst)
	return nil
}

func nodecast(args []string) error {
	if getRpcProxy() == nil {
		return error_code.RpcHostNotConnected
	}
	if len(args) < 1 {
		return error_code.ArgumentError
	}
	mfa, err := rpc.BuildMfa("test", "TestNodeCast", &xgame.TestMsg{Msg: args[0]})
	if err != nil {
		return err
	}
	return rpc.NodeCast(getRpcProxy(), mfa)
}

func getRemotePidList(_ []string) error {
	if getRpcProxy() == nil {
		return error_code.RpcHostNotConnected
	}
	mfa, err := rpc.BuildMfa("test", "GetPidList", &xgame.ReqGetPidList{})
	if err != nil {
		return err
	}
	reply, err := rpc.NodeCall(getRpcProxy(), mfa, time.Second)
	if err != nil {
		return err
	}
	replyMsg := reply.(*xgame.ReplyGetPidList)
	for _, pidBin := range replyMsg.Pids {
		decodePid, err := pid.DecodePid(pidBin)
		if err != nil {
			return err
		}
		remotePidList = append(remotePidList, decodePid)
		log.Infof("pid = %s \n", decodePid)
	}
	return nil
}

func actorCall(args []string) error {
	if len(remotePidList) < 1 || len(args) < 1 {
		return error_code.ArgumentError
	}
	reply, err := process.Call(remotePidList[0], &xgame.TestMsg{Msg: args[0]}, time.Second*3)
	if err != nil {
		return err
	}
	log.Infof("reply = %v", reply)
	return nil
}

func actorCast(args []string) error {
	if len(remotePidList) < 1 || len(args) < 1 {
		return error_code.ArgumentError
	}
	return process.Cast(remotePidList[0], &xgame.TestMsg{Msg: args[0]})
}

func listProxy(_ []string) error {
	proxyMgr := rpc.GetRpcProxyMgr()
	proxyList := proxyMgr.GetAllProxy()
	for _, proxy := range proxyList {
		fmt.Printf("RpcProxy: local = %s, remote = %s\n", proxy.GetLocalHost(), proxy.GetRemoteHost())
	}
	return nil
}

func safeExec(f func([]string) error, args []string) error {
	defer func() {
		if r := recover(); r != nil {
			fmt.Println(r)
			return
		}
	}()
	return f(args)
}
