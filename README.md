# golang & erlang rpc测试


如果需要生成协议，将tool\proto 加入到path中，执行make proto

example
```erlang
    xrpc:start(),
    xrpc:connect(),
    xrpc:node_call(...),
    xrpc:node_cast(...),
    xrpc:actor_call(...),
    xrpc:actor_cast(...),
```
```golang
    xrpc.Start(NodeName)
    xrpc.Connect(),
    xrpc.NodeCall(...)
    xrpc.NodeCast(...)
    xrpc.ActorCall(...)
    xrpc.ActorCast(...)
```

文档待完善，一切以代码为准