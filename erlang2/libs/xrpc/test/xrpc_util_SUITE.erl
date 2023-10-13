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
-define(TEST_EPID, <<188, 0, 0, 0, 1, 0, 22, 122, 104, 97, 110, 103, 116, 117, 111, 64, 49, 50, 55, 46, 48, 46, 48, 46, 49, 64, 49, 50, 51>>).


%% API
all() ->
  [gpid_test, pid_test, pid_test, node_test].

gpid_test(_) ->
  true = xrpc_util:is_gpid_bin(?TEST_EPID),
  {ok, 'zhangtuo@127.0.0.1@123'} = xrpc_util:get_node_by_pid(?TEST_EPID).

pid_test(_) ->
  EpidBin = term_to_binary(self()),
  true = xrpc_util:is_epid_bin(EpidBin),
  Node = node(),
  {ok, Node} = xrpc_util:get_node_by_pid(EpidBin).

pid_encode(_) ->
  TestPid1 = self(),
  EPid1 = xrpc_util:encode_pid(TestPid1),
  {ok, TestPid1} = xrpc_util:decode_pid(EPid1),
  EPid2 = xrpc_util:encode_pid(?TEST_EPID),
  {ok, ?TEST_EPID} = xrpc_util:decode_pid(EPid2).

node_test(_) ->
  {ok, Node} = xrpc_util:get_node_by_pid(?TEST_EPID),
  true = xrpc_util:is_valid_node(Node).

node_test2(_) ->
  Node = xrpc_util:get_node_by_pid(?TEST_EPID),
  true = xrpc_util:is_valid_node(Node),
  {ok, {"127.0.0.1", "123"}} = xrpc_util:get_node_address(Node).

