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

-record(state, {
  last_pid
}).


%% API
-export([test_node_call/1, start/0, init/1, handle_call/3, handle_cast/2, get_pid/1]).

test_node_call(Msg) ->
  io:format("test node call =~p", [Msg]),
  {ok, Msg}.

get_pid(Req) ->
  io:format("get_pid =~p~n", [Req]),
  {ok, Pid} = gen_server:call(?MODULE, get_pid),
  {ok, #'xgame.ReplyGetPidList'{pids = [term_to_binary(Pid)]}}.

start() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  {ok, #state{}}.

handle_call(get_pid, _, State = #state{last_pid = Pid}) ->
  {reply, {ok, Pid}, State};

handle_call({go_msg, FromPid, Msg}, _, State) ->
  io:format("handle_call =~p~n", [Msg]),
  {reply, {ok, Msg}, State#state{last_pid = FromPid}}.

handle_cast({go_msg, Msg}, State) ->
  io:format("handle_cast =~p~n", [Msg]),
  {noreply, State}.


