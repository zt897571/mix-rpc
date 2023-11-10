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
-export([call/5, cast/3]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, handle_continue/2]).

call(Pid, Seq, Flag, ReqBin, Timeout) ->
  case gen_server:call(Pid, {call_remote, Seq, Flag, ReqBin}, Timeout) of
    timeout -> timeout;
    ReplyBin when is_binary(ReplyBin) ->
      packet_util:decode_reply_msg(ReplyBin)
  end.

cast(Pid, Flag, Msg) ->
  gen_server:cast(Pid, {cast_remote, Flag, Msg}).

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
  case gen_tcp:connect(Address, Port, [{packet, 4}, {active, false}]) of
    {ok, Socket} ->
      {ok, Node} = req_verify(Socket),
      ok = gen_tcp:controlling_process(Socket, self()),
      ok = inet:setopts(Socket, ?RPC_SOCKET_OPTS),
      {ok, #state{node = Node, socket = Socket, transport = gen_tcp, seq_map = #{}, verify = true}};
    {error, Reason} ->
      {stop, Reason};
    Other -> log("connect = ~p", [Other])
  end.

terminate(Reason, #state{node = Node}) ->
  log("Terminate = ~p", [Reason]),
  ok = xrpc_register:unregister_node(Node),
  ok.

handle_continue(handshake, State = #state{ref = Ref, transport = Transport}) ->
  {ok, Socket} = ranch:handshake(Ref),
  Transport:setopts(Socket, ?RPC_SOCKET_OPTS),
  {noreply, State#state{socket = Socket}}.

handle_call({call_remote, Seq, Flag, Msg}, From, State = #state{socket = Socket, transport = Transport, seq_map = SeqMap}) ->
  {ok, Binary} = packet_util:encode_packet(Flag, Seq, Msg),
  Transport:send(Socket, Binary),
  {noreply, State#state{seq_map = SeqMap#{Seq => From}}}.


handle_cast({cast_remote, Flag, Msg}, State = #state{socket = Socket, transport = Transport}) ->
  {ok, Binary} = packet_util:encode_packet(Flag, 0, Msg),
  Transport:send(Socket, Binary),
  {noreply, State}.

handle_info({tcp, _, Binary}, State = #state{seq_map = SeqMap, verify = true}) ->
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
        From = {Pid, _} ->
          case is_process_alive(Pid) of
            true ->
              gen_server:reply(From, BinData),
              {noreply, State};
            false ->
              log("process = ~p is not Alive", [Pid]),
              {noreply, State#state{seq_map = maps:remove(Seq, SeqMap)}}
          end
      end
  end;

%% 验证cookie
handle_info({tcp, _, <<Binary/binary>>}, State = #state{verify = false, socket = Socket, transport = Transport}) ->
  try
    {ok, Flag, Seq, Bin} = packet_util:decode_packet(Binary),
    true = packet_util:check_flag(Flag, ?VERIFY_FLAG),
    true = packet_util:check_flag(Flag, ?REQ_FLAG),
    #'xgame.req_verify'{node = Node, cookie = Cookie} = rpc_pb:decode_msg(Bin, 'xgame.req_verify'),
    Cookie2 = list_to_atom(Cookie),
    io:format("req cookie = ~p, local cookie = ~p~n", [Cookie2, erlang:get_cookie()]),
    case Cookie2 == erlang:get_cookie() of
      true ->
        Reply = #'xgame.reply_verify'{node = atom_to_list(xrpc:get_node_name())},
        {ok, BinData} = packet_util:encode_packet(?VERIFY_REPLY_MSG, Seq, rpc_pb:encode_msg(Reply)),
        Transport:send(Socket, BinData),
        AtomNode = packet_util:list_to_atom2(Node),
        ok = xrpc_register:register_node(AtomNode, self()),
        {noreply, State#state{verify = true, node = AtomNode}};
      false ->
        {stop, ?ERROR_COOKIE_VERIFY, State}
    end
  catch
    _ ->
      {stop, ?ERROR_COOKIE_VERIFY, State}
  end;

handle_info({tcp_closed, _}, State) ->
  {stop, normal, State};

handle_info({'EXIT', _, _Reason}, State) ->
%% todo:: zhangtuo retry register pid
  log("Request handleInfo = ~p ~n", []),
  {noreply, State}.

req_verify(Socket) ->
  Req = #'xgame.req_verify'{node = atom_to_list(node()), cookie = atom_to_list(erlang:get_cookie())},
  {ok, BinData} = packet_util:encode_packet(?VERIFY_REQ_MSG, 0, rpc_pb:encode_msg(Req)),
  ok = gen_tcp:send(Socket, BinData),
  {ok, Packet} = gen_tcp:recv(Socket, 0, 5000),
  {ok, Flag, _, Bin} = packet_util:decode_packet(Packet),
  true = packet_util:check_flag(Flag, ?VERIFY_REPLY_MSG),
  #'xgame.reply_verify'{node = Node} = rpc_pb:decode_msg(Bin, 'xgame.reply_verify'),
  AtomNode = packet_util:list_to_atom2(Node),
  ok = xrpc_register:register_node(AtomNode, self()),
  {ok, AtomNode}.


handle_node_req(Flag, Seq, Binary, #state{transport = Transport, socket = Socket}) ->
  Reply = run_pbmfa(Binary),
  case packet_util:check_flag(Flag, ?CALL_FLAG) of
    true ->
      case packet_util:encode_reply_msg(Reply) of
        {ok, ReplyBin} ->
          {ok, BinData} = packet_util:encode_packet(?NODE_REPLY_MSG, Seq, ReplyBin),
          Transport:send(Socket, BinData);
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
        false ->
          packet_util:encode_reply_msg(?ERROR_PID_IS_NOT_ALIVE);
        true ->
          case packet_util:check_flag(Flag, ?CALL_FLAG) of
            false ->
              gen_server:cast(TargetPid, {xrpc, Msg}),
              ignore;
            true ->
              {ok, Rst} = gen_server:call(TargetPid, {xrpc, FromPid, Msg}),
              packet_util:encode_reply_msg(Rst)
          end
      end
    catch _:{_, Error} ->
      log("handle actor req error = ~p ~n", [Error]),
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
  try
    {ok, M, F, A} = packet_util:decode_mfa(Binary),
    F2 = func_name_index:get_function_name(F),
    log("run mfa = ~p ~p ~p~n", [M, F, A]),
    apply(M, F2, [A])
  catch
    Error ->
      log("run mfa err = ~p~n", Error),
      ?ERROR_RUN_MFA_ERROR
  end;
run_pbmfa(_) ->
  ?ERROR_MFA_ARGS.

