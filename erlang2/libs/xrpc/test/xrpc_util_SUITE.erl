%%%-------------------------------------------------------------------
%%% @author zhangtuo
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 10月 2023 14:28
%%%-------------------------------------------------------------------
-module(xrpc_util_SUITE).
-author("zhangtuo").

%% API
-export([]).
-compile(export_all).
-compile(nowarn_export_all).

%% API
all() ->
  [gpid_test, pid_test, pid_test, node_test].

gpid_test(_) ->
  GPidBin = <<188, 0, 0, 0, 1, 0, 22, 122, 104, 97, 110, 103, 116, 117, 111, 64, 49, 50, 55, 46, 48, 46, 48, 46, 49, 64, 49, 50, 51>>,
  true = xrpc_util:is_gpid_bin(GPidBin),
  {ok, 'zhangtuo@127.0.0.1@123'} = xrpc_util:get_node_by_pid(GPidBin).

pid_test(_) ->
  EpidBin = term_to_binary(self()),
  true = xrpc_util:is_epid_bin(EpidBin),
  Node = node(),
  {ok, Node} = xrpc_util:get_node_by_pid(EpidBin).

pid_encode(_) ->
  TestPid1 = self(),
  EPid1 = xrpc_util:encode_pid(TestPid1),
  {ok, TestPid1} = xrpc_util:decode_pid(EPid1),
  TestPid2 = <<188, 0, 0, 0, 1, 0, 22, 122, 104, 97, 110, 103, 116, 117, 111, 64, 49, 50, 55, 46, 48, 46, 48, 46, 49, 64, 49, 50, 51>>,
  EPid2 = xrpc_util:encode_pid(TestPid2),
  {ok, TestPid2} = xrpc_util:decode_pid(EPid2).


node_test(_) ->
  TestPid2 = <<188, 0, 0, 0, 1, 0, 22, 122, 104, 97, 110, 103, 116, 117, 111, 64, 49, 50, 55, 46, 48, 46, 48, 46, 49, 64, 49, 50, 51>>,
  {ok, Node} = xrpc_util:get_node_by_pid(TestPid2),
  true = xrpc_util:is_go_node(Node).


