%%%-------------------------------------------------------------------
%%% @author zhangtuo
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 7月 2023 16:42
%%%-------------------------------------------------------------------
-module(rpc_server).
-author("zhangtuo").

-record(state, {
  listen
}).

%% API
-export([start/1, init/1]).

start(NodeName) ->
  gen_server:start({local, rcp_server}, ?MODULE, [NodeName], []).

init([NodeName]) ->
  Port = 10086,
  node(self()),
  {ok, Socket} = do_listen(Port),
  %% 注册到epmd
  {ok, Address} = inet:sockname(Socket),
  erl_epmd:register_node(NodeName, Address),
  {ok, #state{}}.

do_listen(Port) ->
  case gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}, {backlog, 128}]) of
    {error, eaddrinuse} -> do_listen(Port + 1);
    Other -> Other
  end.


