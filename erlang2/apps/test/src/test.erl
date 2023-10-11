%%%-------------------------------------------------------------------
%%% @author zhangtuo
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 10月 2023 17:55
%%%-------------------------------------------------------------------
-module(test).
-author("zhangtuo").

-include("msg_pb.hrl").
-include("error_code.hrl").

%% API
-export([test_node_call/1]).

test_node_call(Msg) ->
  io:format("test node call =~p", [Msg]),
  {ok, Msg}.