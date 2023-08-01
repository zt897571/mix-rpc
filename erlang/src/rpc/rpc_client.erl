%%%-------------------------------------------------------------------
%%% @author zhangtuo
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 7月 2023 16:16
%%%-------------------------------------------------------------------
-module(rpc_client).
-author("zhangtuo").

-include("rpc_pb.hrl").
-include("msg_pb.hrl").
-include("rpc.hrl").
%% API

-export([connect/2, call/1, call/3, cast/1, cast/2]).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).

-record(state, {
  socket,
  seq = 0,
  wait_map = #{}
}).

connect(Addr, Port) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Addr, Port], []).

call(Msg) ->
  call(Msg, "", ?RPC_TIMEOUT).

call(Msg, Target, Timeout) ->
  Flag = packet_util:build_flag([?REQ_FLAG, ?CALL_FLAG, ?IS_ERLANG_NODE_FLAG]),
  BinData = msg_pb:encode_msg(Msg),
  case gen_server:call(?MODULE, {send_msg, Flag, BinData, Target}, Timeout) of
    {ok, Bin} -> {ok, msg_pb:decode_msg(Bin, test_msg)};
    Other -> Other
  end.

cast(Msg) ->
  cast(Msg, "").

cast(Msg, Target) ->
  Flag = packet_util:build_flag([?REQ_FLAG, ?IS_ERLANG_NODE_FLAG]),
  BinData = msg_pb:encode_msg(Msg),
  gen_server:cast(?MODULE, {send_msg, Flag, BinData, Target, self()}).


init([Addr, Port]) ->
  {ok, Socket} = gen_tcp:connect(Addr, Port, [{packet, 4}, {nodelay, true}]),
  gen_tcp:controlling_process(Socket, self()),
  {ok, #state{socket = Socket}}.

%% 发送消息
handle_call({send_msg, Flag, Msg, Target}, From = {Pid, _}, State = #state{socket = Socket, seq = Seq, wait_map = WaitMap}) ->
  {ok, BinData} = packet_util:pack_req_msg(Msg, Seq, Pid, Target),
  ok = gen_tcp:send(Socket, <<Flag:32/big, BinData/binary>>),
  NewWaitMap =
    case packet_util:check_flag(Flag, ?CALL_FLAG) of
      true -> WaitMap#{Seq => From};
      false -> WaitMap
    end,
  {noreply, State#state{seq = next_seq_id(Seq), wait_map = NewWaitMap}}.
handle_cast({send_msg, Flag, Msg, Target, Pid}, State = #state{socket = Socket}) ->
  {ok, BinData} = packet_util:pack_req_msg(Msg, 0, Pid, Target),
  ok = gen_tcp:send(Socket, <<Flag:32/big, BinData/binary>>),
  {noreply, State}.

%% 接收消息
handle_info({tcp, _, Data}, State = #state{wait_map = WaitMap, socket = Socket}) ->
  <<Flag:32/big, BinData/binary>> = iolist_to_binary(Data),
  case packet_util:check_flag(Flag, ?REQ_FLAG) of
    true ->
      %% 请求消息
      spawn(fun() ->
        ReqMsg = rpc_pb:decode_msg(BinData, req_message),
        {ok, Reply} = gen_server:call(ReqMsg#req_message.target, ReqMsg#req_message.payload, ?RPC_TIMEOUT),
        gen_tcp:send(Reply, Socket)
            end),
      ok;
    false ->
      %% 回复消息
      %%todo:zhangtuo 考虑spawn出进程处理
      Msg = rpc_pb:decode_msg(iolist_to_binary(BinData), reply_message),
      case maps:get(Msg#reply_message.seq, WaitMap, undefined) of
        undefined ->
          io:format("seq = ~p not found waitMap = ~p", [Msg#reply_message.seq, WaitMap]);
        From ->
          gen_server:reply(From, {ok, Msg#reply_message.payload})
      end
  end,
  {noreply, State}.

%% todo zhangtuo 异常关闭tcp处理

next_seq_id(Seq) ->
  Seq + 1 rem ?RPC_SEQ_MAX.



