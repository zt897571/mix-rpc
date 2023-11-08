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
-export([]).
-behaviour(test_node_service).

-record(state, {
  last_pid
}).


%% API
-export([start/0, init/1, handle_call/3, handle_cast/2, test_node/1, on_call_node_test/1, on_cast_node_test/1, on_call_node_get_pid_list/1, on_cast_node_get_pid_list/1]).


%% nodemsg
on_call_node_test(Msg) ->
  io:format("on_call_node_test =~p", [Msg]),
  {ok, Msg}.

on_cast_node_test(Msg) ->
  io:format("on_cast_node_test =~p", [Msg]),
  ok.

on_call_node_get_pid_list(Req) ->
  io:format("on_call_node_get_pid_list =~p~n", [Req]),
  Pid = whereis(?MODULE),
  {ok, #'xgame.ReplyGetPidList'{pids = [term_to_binary(Pid)]}}.

on_cast_node_get_pid_list(Req) ->
  io:format("on_cast_node_get_pid_list =~p", [Req]),
  ok.

start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}}.

handle_call(get_pid, _, State = #state{last_pid = Pid}) ->
  {reply, {ok, Pid}, State};

handle_call({xrpc, FromPid, Msg}, _, State) ->
  io:format("handle_call =~p~n", [Msg]),
  {reply, {ok, Msg}, State#state{last_pid = FromPid}}.

handle_cast({xrpc, Msg}, State) ->
  io:format("handle_cast =~p~n", [Msg]),
  {noreply, State}.

test_node(TargetNode) ->
  Node = packet_util:list_to_atom2(TargetNode),
  ok = xrpc:connect(Node),
  {ok, #'xgame.ReplyGetPidList'{pids = Pids}} = test_node_service:call_node_get_pid_list(Node, "NodeCmd", #'xgame.ReqGetPidList'{}, 5000),
  Pid = hd(Pids),
  ok = test_node_service:cast_node_test(Node, "NodeCmd", #'xgame.test_msg'{}),
  ok = xrpc:node_cast(Node, test, "TestNodeCast", #'xgame.test_msg'{msg = "testMsg"}),
  TestMsg = #'xgame.test_msg'{msg = "testMsg"},
  {ok, TestMsg} = xrpc:actor_call(Pid, TestMsg, 5000),
  ok = xrpc:actor_cast(Pid, TestMsg).



