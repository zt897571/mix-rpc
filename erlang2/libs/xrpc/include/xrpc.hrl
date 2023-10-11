%%%-------------------------------------------------------------------
%%% @author zhangtuo
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 10月 2023 19:35
%%%-------------------------------------------------------------------
-author("zhangtuo").


%% flags
-define(REQ_FLAG, 2#1).     %% 是否为请求
-define(CALL_FLAG, 2#10).   %% 是否为rpc_call
-define(NODEMSG_FLAG, 2#100).  %% 是否为node消息

%% req
-define(NODECALLMSG, 2#111).
-define(NODECASTMSG, 2#101).

-define(ACTORCALLMSG, 2#110).
-define(ACTORCASTMSG, 2#100).

%% reply
-define(NODEREPLYMSG, 2#100).
-define(ACTORREPLYMSG, 2#000).

-define(UNSIGNINT, unsigned-big-integer).

-define(ERLANG_PID_MAGIC_NUM, 131).
-define(GOLANG_PID_MAGIC_NUM, 188).

-define(PID_DIST_TYPE, 88).

-define(IF(BOOL, V1, V2), case BOOL of true -> V1; false -> V2 end).
