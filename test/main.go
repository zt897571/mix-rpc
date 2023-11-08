package main

import (
	"bufio"
	"flag"
	"fmt"
	"golang/common/log"
	xgame "golang/proto"
	"golang/xrpc"
	"os"
	"strings"
	"time"
)

var gHost = flag.String("host", "10.2.58.210:8000", "host")
var nodeName = flag.String("nodeName", "noname", "nodeName")

var gCmd2Func = map[string]func(args []string) error{
	"test": test,
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

func safeExec(f func([]string) error, args []string) error {
	defer func() {
		if r := recover(); r != nil {
			fmt.Println(r)
			return
		}
	}()
	return f(args)
}

func test(args []string) error {
	// 1.
	node := args[0]
	module := args[1]
	err := xrpc.Connect(node)
	if err != nil {
		return err
	}
	replyGetPidList, err := xrpc.CallNodeGetPidList(node, module, &xgame.ReqGetPidList{}, time.Second)
	if err != nil {
		return err
	}
	err = xrpc.CastNodeGetPidList(node, module, &xgame.ReqGetPidList{})
	if err != nil {
		return err
	}
	// 4. actor call test
	pid, err := xrpc.DecodePid(replyGetPidList.Pids[0])
	if err != nil {
		return err
	}
	err = xrpc.Cast(pid, &xgame.TestMsg{Msg: "test cast"})
	if err != nil {
		return err
	}
	_, err = xrpc.CallTest(pid, &xgame.TestMsg{Msg: "test call"}, time.Second*3)
	if err != nil {
		return err
	}
	//
	proxyPid, err := xrpc.CreateProcess(xrpc.NewProxyActor(pid))
	if err != nil {
		return err
	}
	_, err = xrpc.CallTest(proxyPid, &xgame.TestMsg{Msg: "test call"}, time.Second*3)
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
