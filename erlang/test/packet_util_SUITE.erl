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

-include("rpc.hrl").

-compile(export_all).
-compile(nowarn_export_all).

%% API
all() ->
  [flag_test].

flag_test(_) ->
  Flag = packet_util:build_flag([?REQ_FLAG, ?CALL_FLAG]),
  true = packet_util:check_flag(Flag, ?REQ_FLAG),
  true = packet_util:check_flag(Flag, ?CALL_FLAG),
  false = packet_util:check_flag(Flag, ?COMPRESS_FLAG).
