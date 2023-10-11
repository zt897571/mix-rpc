%%%-------------------------------------------------------------------
%%% @author zhangtuo
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 10月 2023 18:36
%%%-------------------------------------------------------------------
-module(xrpc_util).
-author("zhangtuo").

-include("xrpc.hrl").
-include("error_code.hrl").

%% API
-export([is_epid_bin/1, is_gpid_bin/1, is_go_node/1, get_node_by_pid/1, decode_pid/1, encode_pid/1]).

%% 判断是否为erlang pid结构
is_epid_bin(<<?ERLANG_PID_MAGIC_NUM, ?PID_DIST_TYPE, _/binary>>) ->
  true;
is_epid_bin(_) ->
  false.

is_gpid_bin(<<?GOLANG_PID_MAGIC_NUM, _/binary>>) ->
  true;
%% 特殊处理，兼容不在golang actor内call erlang actor的情况
is_gpid_bin(undefined) ->
  true;
is_gpid_bin(_) ->
  false.

is_go_node(Node) when is_atom(Node) ->
  is_go_node(atom_to_list(Node));
is_go_node(Node) when is_list(Node) ->
  length(string:tokens(Node, "@")) == 3.

get_node_by_pid(Binary) when is_binary(Binary) ->
  case decode_pid(Binary) of
    {ok, Pid} when is_pid(Pid) ->
      {ok, node(Pid)};
    {ok, <<?GOLANG_PID_MAGIC_NUM:8/?UNSIGNINT, _:32, _:16/?UNSIGNINT, Node/binary>>} ->
      {ok, binary_to_atom(Node)};
    Error -> Error
  end;
get_node_by_pid(Pid) when is_pid(Pid) ->
  node(Pid).


encode_pid(Pid) when is_pid(Pid) ->
  {ok, term_to_binary(Pid)};
encode_pid(Binary) when is_binary(Binary) ->
  case is_epid_bin(Binary) orelse is_gpid_bin(Binary) of
    true -> {ok, Binary};
    false -> ?ERROR_PID_FORMAT
  end.

decode_pid(Pid) when is_pid(Pid) ->
  {ok, Pid};
decode_pid(Binary) ->
  case is_epid_bin(Binary) of
    true ->
      Pid = binary_to_term(Binary),
      case is_pid(Pid) of
        true -> {ok, Pid};
        false -> ?ERROR_PID_FORMAT
      end;
    false ->
      case is_gpid_bin(Binary) of
        true -> {ok, Binary};
        false -> ?ERROR_PID_FORMAT
      end
  end.
