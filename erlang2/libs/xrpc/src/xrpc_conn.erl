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

-record(state, {
  node :: atom(),
  socket,
  transport,
  ref,
  seq_map = #{}
}).

%% API
-export([start_link/4, start_link/3]).
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


init({Ref, Transport, _}) ->
  process_flag(trap_exit, true),
  {ok, #state{ref = Ref, transport = Transport, seq_map = #{}}, {continue, handshake}}.

start_link(Ref, _Socket, Transport, Opt) ->
  start_link(Ref, Transport, Opt).

start_link(Ref, Transport, Opt) ->
  gen_server:start_link(?MODULE, {Ref, Transport, Opt}, []).

terminate(_, #state{node = Node}) ->
  ok = xrpc_register:unregister_node(Node),
  ok.

handle_continue(handshake, State = #state{ref = Ref, transport = Transport, node = Node}) ->
  {ok, Socket} = ranch:handshake(Ref),
  Transport:setopts(Socket, [{packet, 4}, {active, true}, {nodelay, true}]),
  xrpc_register:register_node(Node, self()),
  {noreply, State#state{socket = Socket}}.

handle_call({call_remote, Seq, Flag, Msg}, From, State = #state{socket = Socket, transport = Transport, seq_map = SeqMap}) ->
  send_msg(Transport, Socket, Flag, Seq, Msg),
  {noreply, State#state{seq_map = SeqMap#{Seq => From}}}.


handle_cast({cast_remote, Flag, Msg}, State = #state{socket = Socket, transport = Transport}) ->
  send_msg(Transport, Socket, Flag, 0, Msg),
  {noreply, State}.

handle_info({tcp, _, <<Flag:16/?UNSIGNINT, Seq:32/?UNSIGNINT, BinData/binary>>}, State = #state{socket = Socket, transport = Transport, seq_map = SeqMap}) ->
  case packet_util:check_flag(Flag, ?REQ_FLAG) of
    true ->
      case packet_util:check_flag(Flag, ?NODEMSG_FLAG) of
        true -> handle_node_req(Flag, Seq, BinData, Socket, Transport);
        false -> handle_actor_req(Flag, Seq, BinData, Socket, Transport)
      end;
    false ->
      case maps:get(Seq, SeqMap, undefined) of
        undefined -> log("Seq not found wait pid =~p", [Seq]);
        Pid ->
          case is_process_alive(Pid) of
            true -> gen_server:reply(Pid, BinData);
            false -> log("process = ~p is not Alive", [Pid])
          end
      end
  end,
  {noreply, State};


handle_info({'EXIT', _, _Reason}, State) ->
  %% todo:: zhangtuo retry register pid
  log("Request handleInfo = ~p ~n", []),
  {noreply, State}.

handle_node_req(Flag, Seq, Binary, Socket, Transport) ->
  Reply = run_pbmfa(Binary),
  case packet_util:check_flag(Flag, ?CALL_FLAG) of
    true ->
      case packet_util:encode_reply_msg(Reply) of
        {ok, ReplyBin} ->
          send_msg(Transport, Socket, ?NODEREPLYMSG, Seq, ReplyBin);
        Other ->
          log("build reply msg error = ~p", [Other])
      end;
    false ->
      ignore
  end.

handle_actor_req(Flag, Seq, BinData, Socket, Transport) ->
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
    {ok, ReplyBin} -> send_msg(Transport, Socket, ?ACTORREPLYMSG, Seq, ReplyBin)
  end.

send_msg(Transport, Socket, Flag, Seq, BinData) ->
  Transport:send(Socket, <<Flag:16/?UNSIGNINT, Seq:32/?UNSIGNINT, BinData/binary>>).

log(Format, Args) ->
  io:format(Format, Args).

run_pbmfa(Binary) when is_binary(Binary) ->
  {ok, M, F, A} = packet_util:decode_mfa(Binary),
  apply(M, F, [A]);
run_pbmfa(_) ->
  ?ERROR_MFA_ARGS.
