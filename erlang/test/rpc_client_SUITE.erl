%%%-------------------------------------------------------------------
%%% @author zhangtuo
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 7月 2023 18:50
%%%-------------------------------------------------------------------
-module(rpc_client_SUITE).
-author("zhangtuo").

-include("msg_pb.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).
-compile(nowarn_export_all).

all() ->
  [].
%%  [{group, group1}].

groups() ->
  [{group1, [sequence], [call_test, cast_test, seq_call_test]}].

init_per_suite(Config) ->
  {ok, _} = rpc_client:connect("localhost", 8000),
  Config.

end_per_suite(Config) ->
  Config.

call_test(_) ->
  Msg = #test_msg{msg = "Test", delayTime = 0},
  {ok, Msg} = rpc_client:call(Msg, "test").

cast_test(_) ->
  Msg = #test_msg{msg = "Test", delayTime = 0},
  ok = rpc_client:cast(Msg, "test").

seq_call_test(_) ->
  Msg1 = #test_msg{msg = "3", delayTime = 3},
  Msg2 = #test_msg{msg = "2", delayTime = 2},
  Msg3 = #test_msg{msg = "1", delayTime = 1},
  spawn(fun() -> {ok, "3"} = rpc_client:call(Msg1, "test") end),
  spawn(fun() -> {ok, "2"} = rpc_client:call(Msg2, "test") end),
  spawn(fun() -> {ok, "1"} = rpc_client:call(Msg3, "test") end).


