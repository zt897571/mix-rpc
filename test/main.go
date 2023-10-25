package main

import (
	"bufio"
	"flag"
	"fmt"
	"golang/common/log"
	"golang/error_code"
	iface2 "golang/iface"
	xgame "golang/proto"
	"golang/xrpc"
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
	"getRemotePidList": getRemotePidList,
	"actorCall":        actorCall,
	"actorCast":        actorCast,
	"testErlang":       testErlang,
	"testGo":           testGo,
}

func main() {
	flag.Parse()
	nodeName2 := strings.Join([]string{*nodeName, *gHost}, "@")
	err := xrpc.Start(nodeName2, "test_cookie")
	if err != nil {
		return
	}
	log.Infof("Nodename = %s", nodeName2)
	xrpc.RegisterNodeMsg("test", &NodeCmd{})

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

func connect(args []string) error {
	targetNode := args[0]
	err := xrpc.Connect(targetNode)
	if err != nil {
		return err
	}
	return nil
}

func nodecall(args []string) error {
	if len(args) < 2 {
		return error_code.ArgumentError
	}
	mfa, err := xrpc.BuildMfa("test", "test_node_call", &xgame.TestMsg{Msg: args[1]})
	if err != nil {
		return err
	}
	rst, err := xrpc.NodeCall(args[0], mfa, time.Second)
	if err != nil {
		return err
	}
	fmt.Printf("%v\n", rst)
	return nil
}

func nodecast(args []string) error {
	if len(args) < 1 {
		return error_code.ArgumentError
	}
	//mfa, err := xrpc.BuildMfa("test", "TestNodeCast", &xgame.TestMsg{Msg: args[1]})
	mfa, err := xrpc.BuildMfa("test", "test_node_cast", &xgame.TestMsg{Msg: args[1]})
	if err != nil {
		return err
	}
	return xrpc.NodeCast(args[0], mfa)
}

func getRemotePidList(args []string) error {
	//mfa, err := xrpc.BuildMfa("test", "GetPidList", &xgame.ReqGetPidList{})
	mfa, err := xrpc.BuildMfa("test", "get_pid", &xgame.ReqGetPidList{})
	if err != nil {
		return err
	}
	reply, err := xrpc.NodeCall(args[0], mfa, time.Second)
	if err != nil {
		return err
	}
	replyMsg := reply.(*xgame.ReplyGetPidList)
	remotePidList = remotePidList[:0]
	for _, pidBin := range replyMsg.Pids {
		decodePid, err := xrpc.DecodePid(pidBin)
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
	reply, err := xrpc.Call(remotePidList[0], &xgame.TestMsg{Msg: args[0]}, time.Second*3)
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
	return xrpc.Cast(remotePidList[0], &xgame.TestMsg{Msg: args[0]})
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

func testErlang(args []string) error {
	node := args[0]
	// 1.
	err := xrpc.Connect(node)
	if err != nil {
		return err
	}
	// 2. node call test
	mfa, err := xrpc.BuildMfa("test", "get_pid", &xgame.ReqGetPidList{})
	if err != nil {
		return err
	}
	reqReply, err := xrpc.NodeCall(node, mfa, time.Second)
	if err != nil {
		return err
	}
	// 3. node cast test
	err = xrpc.NodeCast(node, mfa)
	if err != nil {
		return err
	}
	// 4. actor call test
	var replyGetPidList *xgame.ReplyGetPidList
	var ok bool
	if replyGetPidList, ok = reqReply.(*xgame.ReplyGetPidList); !ok {
		return error_code.LogicError
	}
	pid, err := xrpc.DecodePid(replyGetPidList.Pids[0])
	if err != nil {
		return err
	}
	err = xrpc.Cast(pid, &xgame.TestMsg{Msg: "test cast"})
	if err != nil {
		return err
	}
	_, err = xrpc.Call(pid, &xgame.TestMsg{Msg: "test call"}, time.Second*3)
	if err != nil {
		return err
	}
	//
	proxyPid, err := xrpc.CreateProcess(&ProxyActor{pid: pid})
	if err != nil {
		return err
	}
	_, err = xrpc.Call(proxyPid, &xgame.TestMsg{Msg: "test call"}, time.Second*3)
	if err != nil {
		return err
	}
	err = xrpc.Cast(proxyPid, &xgame.TestMsg{Msg: "test cast"})
	if err != nil {
		return err
	}
	xrpc.StopProcess(proxyPid)
	return nil
}

func testGo(args []string) error {
	node := args[0]
	// 1.
	err := xrpc.Connect(node)
	if err != nil {
		return err
	}
	// 2. node call test
	mfa, err := xrpc.BuildMfa("test", "GetPidList", &xgame.ReqGetPidList{})
	if err != nil {
		return err
	}
	reqReply, err := xrpc.NodeCall(node, mfa, time.Second)
	if err != nil {
		return err
	}
	// 3. node cast test
	err = xrpc.NodeCast(node, mfa)
	if err != nil {
		return err
	}
	// 4. actor call test
	var replyGetPidList *xgame.ReplyGetPidList
	var ok bool
	if replyGetPidList, ok = reqReply.(*xgame.ReplyGetPidList); !ok {
		return error_code.LogicError
	}
	pid, err := xrpc.DecodePid(replyGetPidList.Pids[0])
	if err != nil {
		return err
	}
	err = xrpc.Cast(pid, &xgame.TestMsg{Msg: "test cast"})
	if err != nil {
		return err
	}
	_, err = xrpc.Call(pid, &xgame.TestMsg{Msg: "test call"}, time.Second*3)
	if err != nil {
		return err
	}
	//
	proxyPid, err := xrpc.CreateProcess(&ProxyActor{pid: pid})
	if err != nil {
		return err
	}
	_, err = xrpc.Call(proxyPid, &xgame.TestMsg{Msg: "test call"}, time.Second*3)
	if err != nil {
		return err
	}
	err = xrpc.Cast(proxyPid, &xgame.TestMsg{Msg: "test cast"})
	if err != nil {
		return err
	}
	xrpc.StopProcess(proxyPid)
	return nil
}
