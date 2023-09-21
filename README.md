# golang & erlang rpc测试

1. make init
2. 启动golang/app/testnode/main   go run golang/app/testnode/main.go
3. make start_erlang 启动console，输入指令

目前仅实现了golang作为服务器使用，erlang作为客户端使用的功能，待后续扩展 
如果需要生成协议，将tool\proto 加入到path中，执行make proto

example
```erlang
    rpc_client:connect("localhost", 8000),
    rpc_client:call(#test_msg{msg = "TestMsg"}).
```


相关文档 https://lilithgames.feishu.cn/wiki/X9Bgw4rdriqVHGk69vgcPdCcnOh
文档待完善，一切以代码为准