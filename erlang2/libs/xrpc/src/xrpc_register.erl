%%%-------------------------------------------------------------------
%%% @author zhangtuo
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 9月 2023 10:36
%%%-------------------------------------------------------------------
-module(xrpc_register).
-author("zhangtuo").
-include("error_code.hrl").

-behavior(gen_server).

%% API
-export([next_seq/1, unregister_node/1, register_node/2]).
-export([start_link/0, init/1, handle_call/3, get_conn_pid_by_node/1, handle_info/2, handle_cast/2, terminate/2]).
-record(node_info, {
  node :: atom(),
  seq :: integer(),
  pid
}).

-record(state, {
  nodeMap :: #{}
}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
%%  process_flag(trap_exit, true),
  {ok, #state{nodeMap = #{}}}.

next_seq(Node) ->
  gen_server:call(?MODULE, {get_next_seq, Node}).

get_conn_pid_by_node(Node) ->
  gen_server:call(?MODULE, {get_conn_pid, Node}).

register_node(Node, Pid) ->
  gen_server:call(?MODULE, {register_node, Node, Pid}).

unregister_node(Node) ->
  gen_server:call(?MODULE, {unregister_node, Node}).

handle_call({register_node, Node, Pid}, _, State = #state{nodeMap = NodeMap}) ->
  NewNodeInfo =
    case maps:get(Node, NodeMap, undefined) of
      undefined ->
        link(Pid),
        #node_info{node = Node, pid = Pid, seq = 0};
      #node_info{} = NodeInfo ->
        io:format("Node repeated register = ~p", [Node]),
        NodeInfo#node_info{pid = Pid}
    end,
  {reply, ok, State#state{nodeMap = NodeMap#{Node => NewNodeInfo}}};

handle_call({unregister_node, Node}, _, State = #state{nodeMap = NodeMap}) ->
  case maps:get(Node, NodeMap, undefined) of
    undefined ->
      {reply, ok, State};
    #node_info{pid = Pid} ->
      unlink(Pid),
      {reply, ok, State#state{nodeMap = maps:remove(Node, NodeMap)}}
  end;

handle_call({get_next_seq, Node}, _, State = #state{nodeMap = NodeMap}) ->
  case maps:get(Node, NodeMap, undefined) of
    #node_info{seq = Seq, pid = Pid} = NodeInfo ->
      NewSeq = Seq + 1,
      NewNodeMap = NodeMap#{Node => NodeInfo#node_info{seq = NewSeq}},
      {reply, {ok, NewSeq, Pid}, State#state{nodeMap = NewNodeMap}};
    undefined ->
      %% todo zhangtuo try connect
      {reply, ?ERROR_NODE_NOT_CONNECTED, State}
  end;

handle_call({get_conn_pid, Node}, _, State = #state{nodeMap = NodeMap}) ->
  case maps:get(Node, NodeMap, undefined) of
    #node_info{pid = Pid} -> {reply, {ok, Pid}, State};
    undefined -> {reply, ?ERROR_NODE_NOT_CONNECTED, State}
  end.

handle_info({'EXIT', Pid, _Reason}, State = #state{nodeMap = NodeMap}) ->
  NodeTpList = maps:to_list(NodeMap),
  [Node] = [Node || {Node, #node_info{pid = Pid1}} <- NodeTpList, Pid =:= Pid1],
  {ok, State = #state{nodeMap = maps:remove(Node, NodeMap)}}.

terminate(Reason, State) ->
  io:format("xrpc_register terminate reason = ~p, state = ~p", [Reason, State]),
  ok.

handle_cast(_, _) ->
  erlang:error(not_implemented).