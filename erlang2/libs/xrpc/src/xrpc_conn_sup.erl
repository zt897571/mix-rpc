%%%-------------------------------------------------------------------
%%% @author zhangtuo
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 10月 2023 17:07
%%%-------------------------------------------------------------------
-module(xrpc_conn_sup).
-author("zhangtuo").

%% API
-export([start_link/0, init/1, start_child/3, start_child/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Ref, Transport, Opts) ->
  supervisor:start_child(?MODULE, [Ref, Transport, Opts]).

start_child(Node) ->
  supervisor:start_child(?MODULE, [Node]).

init([]) ->
  {ok, {{simple_one_for_one, 10, 60},
    [
      {xrpc_conn, {xrpc_conn, start_link, []},
        temporary, 5000, worker, [xrpc_conn]}
    ]}}.
