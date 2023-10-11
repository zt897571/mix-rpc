%%%-------------------------------------------------------------------
%%% @author zhangtuo
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 7月 2023 16:12
%%%-------------------------------------------------------------------
-module(packet_util_SUITE).
-author("zhangtuo").

-include("xrpc.hrl").
-include("msg_pb.hrl").

-compile(export_all).
-compile(nowarn_export_all).

%% API
all() ->
  [flag_test, encode_mfa_test, encode_process_msg_test, encode_reply_test].

flag_test(_) ->
  Flag = packet_util:build_flag([?REQ_FLAG, ?CALL_FLAG]),
  true = packet_util:check_flag(Flag, ?REQ_FLAG),
  true = packet_util:check_flag(Flag, ?CALL_FLAG).

encode_mfa_test(_) ->
  Func = test,
  TestMsg = #'xgame.test_msg'{rand = 10086},
  {ok, MsgBin} = packet_util:encode_mfa(?MODULE, Func, TestMsg),
  {ok, ?MODULE, Func, TestMsg} = packet_util:decode_mfa(MsgBin).

encode_process_msg_test(_) ->
  SelfPid = self(),
  TestMsg = #'xgame.test_msg'{rand = 10086},
  GPidBin = <<188, 0, 0, 0, 1, 0, 22, 122, 104, 97, 110, 103, 116, 117, 111, 64, 49, 50, 55, 46, 48, 46, 48, 46, 49, 64, 49, 50, 51>>,
  true = xrpc_util:is_gpid_bin(GPidBin),
  {ok, 'zhangtuo@127.0.0.1@123'} = xrpc_util:get_node_by_pid(GPidBin),
  {ok, MsgBin} = packet_util:encode_process_req(GPidBin, SelfPid, TestMsg),
  {ok, GPidBin, SelfPid, TestMsg} = packet_util:decode_process_req(MsgBin).

encode_reply_test(_) ->
  ErrorCode = test,
  TestMsg1 = #'xgame.test_msg'{rand = 10086},
  TestMsg2 = #'xgame.test_msg'{rand = 10087},
  {ok, Bin1} = packet_util:encode_reply_msg(ErrorCode),
  {ok, Bin2} = packet_util:encode_reply_msg({ok, TestMsg1}),
  {ok, Bin3} = packet_util:encode_reply_msg(TestMsg2),
  ErrorCode = packet_util:decode_reply_msg(Bin1),
  {ok, TestMsg1} = packet_util:decode_reply_msg(Bin2),
  {ok, TestMsg2} = packet_util:decode_reply_msg(Bin3).
