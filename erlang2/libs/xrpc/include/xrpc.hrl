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
-define(VERIFY_FLAG, 2#1000).  %% 是否为验证消息
%% req
-define(NODECALLMSG, 2#111).
-define(NODECASTMSG, 2#101).

-define(ACTORCALLMSG, 2#011).
-define(ACTORCASTMSG, 2#001).
-define(VERIFY_REQ_MSG, 2#1001).

%% reply
-define(NODE_REPLY_MSG, 2#100).
-define(ACTOR_REPLY_MSG, 2#000).
-define(VERIFY_REPLY_MSG, 2#1000).

-define(UNSIGNINT, unsigned-big-integer).

-define(ERLANG_PID_MAGIC_NUM, 131).
-define(GOLANG_PID_MAGIC_NUM, 188).

-define(PID_DIST_TYPE, 88).

