%%%-------------------------------------------------------------------
%%% @author zhangtuo
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 10月 2023 18:22
%%%-------------------------------------------------------------------
-module(xrpc_SUITE).
-author("zhangtuo").

-include("xrpc.hrl").
-include("msg_pb.hrl").

-compile(export_all).
-compile(nowarn_export_all).

%% API
all() ->
  [test_rpc].

test(TestMsg = #'xgame.test_msg'{}) ->
  TestMsg.

test_rpc(_) ->
  {ok, Pid} = test_process:start_link(),
  ok = xrpc:start(),
  Node = xrpc:get_node_name(),
  ok = xrpc:connect(Node),
  TestMsg = #'xgame.test_msg'{rand = 10086},
  {ok, TestMsg} = xrpc:node_call(Node, ?MODULE, test, [TestMsg], 1000),
  ok = xrpc:node_cast(Node, ?MODULE, test, [TestMsg]),
  {ok, TestMsg} = xrpc:actor_call(Pid, TestMsg, 1000),
  ok = xrpc:actor_cast(Pid, TestMsg),
  ok.