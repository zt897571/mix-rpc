%%%-------------------------------------------------------------------
%%% @author zhangtuo
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 7月 2023 15:50
%%%-------------------------------------------------------------------
-author("zhangtuo").

-define(RPC_TIMEOUT, 5000).
-define(RPC_SEQ_MAX, 1000000).

%% flags
-define(REQ_FLAG, 2#1).     %% 是否为请求
-define(CALL_FLAG, 2#10).   %% 是否为rpc_call
-define(NODEMSG_FLAG, 2#100).  %% 是否为node消息
-define(IS_ERLANG_NODE_FLAG, 2#1000).      %% 是否为erlang节点

%% ...
-define(COMPRESS_FLAG, 2#1000). %% 数据是否压缩...
