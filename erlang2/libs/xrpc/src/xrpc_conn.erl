%%%-------------------------------------------------------------------
%%% @author zhangtuo
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 9月 2023 11:39
%%%-------------------------------------------------------------------
-module(xrpc_conn).
-author("zhangtuo").

-include("xrpc.hrl").
-include("rpc_pb.hrl").
-include("msg_pb.hrl").
-include("error_code.hrl").

-define(RPC_SOCKET_OPTS, [{packet, 4}, {active, true}, {nodelay, true}]).
-record(state, {
  node :: atom(),
  socket,
  transport,
  ref,
  seq_map = #{},
  verify = false
}).

%% API
-export([start_link/4, start_link/3, start_link/1]).
-export([call/4, cast/2]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, handle_continue/2]).

call(Pid, Seq, Msg, Timeout) ->
  case gen_server:call(Pid, {call_remote, Seq, ?NODECALLMSG, Msg}, Timeout) of
    timeout -> timeout;
    MsgBin when is_binary(MsgBin) ->
      packet_util:decode_reply_msg(MsgBin)
  end.

cast(Pid, Msg) ->
  gen_server:cast(Pid, {cast_remote, ?NODECALLMSG, msg_pb:encode_msg(Msg)}).

start_link(Ref, _Socket, Transport, Opt) ->
  start_link(Ref, Transport, Opt).

start_link(Ref, Transport, Opt) ->
  gen_server:start_link(?MODULE, {Ref, Transport, Opt}, []).

start_link(Node) ->
  gen_server:start(?MODULE, [Node], []).

init({Ref, Transport, _}) ->
  process_flag(trap_exit, true),
  {ok, #state{ref = Ref, transport = Transport, seq_map = #{}}, {continue, handshake}};

init([Node]) ->
  {ok, {Address, Port}} = xrpc_util:get_node_address(Node),
  case gen_tcp:connect(Address, list_to_integer(Port), ?RPC_SOCKET_OPTS) of
    {ok, Socket} ->
      Req = #'xgame.req_verify'{node = atom_to_list(node()), cookie = atom_to_list(erlang:get_cookie())},
      {ok, BinData} = packet_util:encode_packet(?VERIFY_REQ_MSG, 0, rpc_pb:encode_msg(Req)),
      ok = gen_tcp:send(Socket, BinData),
      {ok, #state{node = Node, socket = Socket, transport = gen_tcp, seq_map = #{}, verify = false}};
    {error, Reason} ->
      {stop, Reason};
    Other -> log("connect = ~p", [Other])
  end.

terminate(Reason, _) ->
  log("Terminate = ~p", [Reason]),
  ok.

handle_continue(handshake, State = #state{ref = Ref, transport = Transport, node = Node}) ->
  {ok, Socket} = ranch:handshake(Ref),
  Transport:setopts(Socket, ?RPC_SOCKET_OPTS),
  xrpc_register:register_node(Node, self()),
  {noreply, State#state{socket = Socket}}.

handle_call({call_remote, Seq, Flag, Msg}, From, State = #state{socket = Socket, transport = Transport, seq_map = SeqMap}) ->
  {ok, Binary} = packet_util:encode_packet(Flag, Seq, Msg),
  Transport:send(Socket, Binary),
  {noreply, State#state{seq_map = SeqMap#{Seq => From}}}.


handle_cast({cast_remote, Flag, Msg}, State = #state{socket = Socket, transport = Transport}) ->
  {ok, Binary} = packet_util:encode_packet(Flag, 0, Msg),
  Transport:send(Socket, Binary),
  {noreply, State}.

handle_info({tcp, _, <<Binary/binary>>}, State = #state{seq_map = SeqMap, verify = true}) ->
  {ok, Flag, Seq, BinData} = packet_util:decode_packet(Binary),
  case packet_util:check_flag(Flag, ?REQ_FLAG) of
    true ->
      case packet_util:check_flag(Flag, ?NODEMSG_FLAG) of
        true -> handle_node_req(Flag, Seq, BinData, State);
        false -> handle_actor_req(Flag, Seq, BinData, State)
      end,
      {noreply, State};
    false ->
      case maps:get(Seq, SeqMap, undefined) of
        undefined ->
          log("Seq not found wait pid =~p", [Seq]),
          {noreply, State};
        Pid ->
          case is_process_alive(Pid) of
            true ->
              gen_server:reply(Pid, BinData),
              {noreply, State};
            false ->
              log("process = ~p is not Alive", [Pid]),
              {noreply, State#state{seq_map = maps:remove(Seq, SeqMap)}}
          end
      end
  end;

%% 验证cookie
handle_info({tcp, _, Binary}, State = #state{verify = false, socket = Socket, transport = Transport, node = UNode}) ->
  try
    io:format("verify = ~p~n", [Binary]),
    {ok, Flag, Seq, Bin} = packet_util:decode_packet(Binary),
    true = packet_util:check_flag(Flag, ?VERIFY_FLAG),
    case packet_util:check_flag(Flag, ?REQ_FLAG) of
      true ->
        #'xgame.req_verify'{node = Node, cookie = Cookie} = rpc_pb:decode_msg(Bin, 'xgame.req_verify'),
        Cookie2 = list_to_atom(Cookie),
        Cookie2 = erlang:get_cookie(),
        Reply = #'xgame.reply_verify'{node = atom_to_list(node())},
        {ok, BinData} = packet_util:encode_packet(?VERIFY_REPLY_MSG, Seq, rpc_pb:encode_msg(Reply)),
        Transport:send(Socket, BinData),
        ok = xrpc_register:register_node(Node, self()),
        {noreply, State#state{verify = true, node = Node}};
      false ->
        #'xgame.reply_verify'{node = Node} = rpc_pb:decode_msg(Bin, 'xgame.reply_verify'),
        UNode = list_to_atom(Node),
        ok = xrpc_register:register_node(Node, self()),
        {noreply, State#state{verify = true}}
    end
  catch
    _ ->
      {stop, ?ERROR_COOKIE_VERIFY, State}
  end;

handle_info({'EXIT', _, _Reason}, State) ->
%% todo:: zhangtuo retry register pid
  log("Request handleInfo = ~p ~n", []),
  {noreply, State}.

handle_node_req(Flag, Seq, Binary, #state{transport = Transport, socket = Socket}) ->
  Reply = run_pbmfa(Binary),
  case packet_util:check_flag(Flag, ?CALL_FLAG) of
    true ->
      case packet_util:encode_reply_msg(Reply) of
        {ok, ReplyBin} ->
          {ok, Binary} = packet_util:encode_packet(?NODE_REPLY_MSG, Seq, ReplyBin),
          Transport:send(Socket, Binary);
        Other ->
          log("build reply msg error = ~p", [Other])
      end;
    false ->
      ignore
  end.

handle_actor_req(Flag, Seq, BinData, #state{socket = Socket, transport = Transport}) ->
  Result =
    try
      {ok, TargetPid, FromPid, Msg} = packet_util:decode_process_req(BinData),
      case is_process_alive(TargetPid) of
        false -> ?ERROR_PID_IS_NOT_ALIVE;
        true ->
          case packet_util:check_flag(Flag, ?CALL_FLAG) of
            false ->
              gen_server:cast(TargetPid, {go_msg, FromPid, Msg}),
              ignore;
            true ->
              {ok, Rst} = gen_server:call(TargetPid, {go_msg, FromPid, Msg}),
              packet_util:encode_reply_msg(Rst)
          end
      end
    catch _:{_, Error} ->
      packet_util:encode_reply_msg(Error)
    end,
  case Result of
    ignore -> ok;
    {ok, ReplyBin} ->
      {ok, Binary} = packet_util:encode_packet(?ACTOR_REPLY_MSG, Seq, ReplyBin),
      Transport:send(Socket, Binary)
  end.

log(Format, Args) ->
  io:format(Format, Args).

run_pbmfa(Binary) when is_binary(Binary) ->
  {ok, M, F, A} = packet_util:decode_mfa(Binary),
  apply(M, F, [A]);
run_pbmfa(_) ->
  ?ERROR_MFA_ARGS.