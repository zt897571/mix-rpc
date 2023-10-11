%%%-------------------------------------------------------------------
%%% @author zhangtuo
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 10月 2023 17:46
%%%-------------------------------------------------------------------
-module(xrpc_protocol).
-author("zhangtuo").

-behavior(ranch_protocol).
%% API
-export([start_link/4, start_link/3]).

start_link(Ref, _Socket, Transport, Opt) ->
  start_link(Ref, Transport, Opt).

start_link(Ref, Transport, Opts) ->
  io:format("Connection  = ~p ~p ~p ~n", [Ref, Transport, Opts]),
  xrpc_conn_sup:start_child(Ref, Transport, Opts).

