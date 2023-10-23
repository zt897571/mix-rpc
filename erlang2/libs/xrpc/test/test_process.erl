%%%-------------------------------------------------------------------
%%% @author zhangtuo
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 10月 2023 15:19
%%%-------------------------------------------------------------------
-module(test_process).
-author("zhangtuo").

%% API
-export([init/1, handle_call/3, handle_cast/2, start_link/0, stop/0]).

-include("xrpc.hrl").
-behavior(gen_server).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

stop() ->
  gen_server:stop(?MODULE).

init([]) ->
  {ok, []}.

handle_call({go_msg, _, Msg}, _, State) ->
  {reply, {ok, Msg}, State}.

handle_cast({go_msg, _}, State) ->
  {noreply, State}.