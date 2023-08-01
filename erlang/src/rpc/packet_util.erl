%%%-------------------------------------------------------------------
%%% @author zhangtuo
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 7月 2023 15:56
%%%-------------------------------------------------------------------
-module(packet_util).
-author("zhangtuo").

-include("msg_pb.hrl").
-include("rpc_pb.hrl").
%% API
-export([check_flag/2, build_flag/1, pack_req_msg/4]).


build_flag(FlagList) when is_list(FlagList) ->
  lists:foldl(
    fun(F, Acc) ->
      Acc bor F
    end, 0, FlagList).

check_flag(Bin, Flag) when is_integer(Bin) andalso is_integer(Flag) ->
  Bin band Flag =:= Flag.

pack_req_msg(BinData, Seq, FromPid, Target) when is_binary(BinData) andalso is_pid(FromPid) andalso is_list(Target) ->
  ReqMsg = #req_message{
    seq = Seq,
    payload = BinData,
    target = Target,
    source = pid_to_list(FromPid)
  },
  {ok, rpc_pb:encode_msg(ReqMsg)}.

