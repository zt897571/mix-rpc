%%%-------------------------------------------------------------------
%%% @author zhangtuo
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 9. 27月 2023 11:15
%%%-------------------------------------------------------------------
-module(xrpc).
-author("zhangtuo").

%% API
-export([]).
-include("xrpc.hrl").
-include("rpc_pb.hrl").
-include("error_code.hrl").

-export([start/0, node_call/5, node_cast/4, actor_call/3, actor_cast/2]).

start() ->
  {ok, _} = xrpc_conn_sup:start_link(),
  {ok, _} = ranch:start_listener(xrpc_protocol, ranch_tcp, #{socket_opts => [{port, 0}]}, xrpc_protocol, []),
  %% todo zhangtuo 接入服务发现流程
  io:format("start port = ~p", [ranch:get_port(xrpc_protocol)]).


node_call(Node, Module, Fun, Args, Timeout) ->
  case xrpc_util:is_go_node(Node) of
    false -> rpc:call(Node, Module, Fun, Args, Timeout);
    true ->
      case xrpc_register:next_seq(Node) of
        {ok, Seq, Pid} ->
          {ok, ReqBin} = packet_util:encode_mfa(Module, Fun, Args),
          xrpc_conn:call(Pid, Seq, ReqBin, Timeout);
        Error ->
          Error
      end
  end.

node_cast(Node, Module, Fun, Args) ->
  case xrpc_util:is_go_node(Node) of
    false -> rpc:cast(Node, Module, Fun, Args);
    true ->
      case xrpc_register:get_conn_by_node(Node) of
        {ok, Pid} ->
          {ok, ReqBin} = packet_util:encode_mfa(Module, Fun, Args),
          xrpc_conn:cast(Pid, ReqBin);
        Error -> Error
      end
  end.

actor_call(Pid, Msg, Timeout) ->
  case is_pid(Pid) of
    true -> gen_server:call(Pid, Msg, Timeout);
    false ->
      case xrpc_util:is_gpid_bin(Pid) of
        false -> ?ERROR_PID_FORMAT;
        true ->
          {ok, Node} = xrpc_util:get_node_by_pid(Pid),
          case xrpc_register:next_seq(Node) of
            {ok, Seq, ConnPid} ->
              {ok, ReqMsgBin} = packet_util:encode_process_req(Msg, Pid, self()),
              xrpc_conn:call(ConnPid, Seq, ReqMsgBin, Timeout);
            Error -> Error
          end
      end
  end.

actor_cast(Pid, Msg) ->
  case is_pid(Pid) of
    true -> gen_server:cast(Pid, Msg);
    false ->
      case xrpc_util:is_gpid_bin(Pid) of
        false -> ?ERROR_PID_FORMAT;
        true ->
          {ok, Node} = xrpc_util:get_node_by_pid(Pid),
          case xrpc_register:get_conn_by_node(Node) of
            {ok, ConnPid} ->
              {ok, ReqMsgBin} = packet_util:encode_process_req(Msg, Pid, self()),
              xrpc_conn:cast(ConnPid, ReqMsgBin);
            Error -> Error
          end
      end
  end.