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

-include("rpc_pb.hrl").
-include("xrpc.hrl").
-include("error_code.hrl").

%% API
-export([check_flag/2, build_flag/1, encode_reply_msg/1, decode_reply_msg/1]).
-export([encode_mfa/3, decode_mfa/1, encode_process_req/3, decode_process_req/1, decode_packet/1, encode_packet/3]).

build_flag(FlagList) when is_list(FlagList) ->
  lists:foldl(
    fun(F, Acc) ->
      Acc bor F
    end, 0, FlagList).

check_flag(Fg, Flag) when is_integer(Fg) andalso is_integer(Flag) ->
  Fg band Flag =:= Flag.

encode_reply_msg(ErrorCode) when is_atom(ErrorCode) ->
  ReplyBin = rpc_pb:encode_msg(#'xgame.reply_message'{error = atom_to_list2(ErrorCode)}),
  {ok, ReplyBin};
encode_reply_msg({ok, Rst}) when is_tuple(Rst) ->
  encode_reply_msg(Rst);
encode_reply_msg(Rst) when is_tuple(Rst) ->
  case msg_pb:verify_msg(Rst) of
    ok ->
      ReplyMsg = #'xgame.reply_message'{msgName = atom_to_list(element(1, Rst)), payload = msg_pb:encode_msg(Rst)},
      ReplyBin = rpc_pb:encode_msg(ReplyMsg),
      {ok, ReplyBin};
    Error -> Error
  end;
encode_reply_msg(_) ->
  ?ERROR_REPLY_MSG_TYPE.

decode_msg_by_name(MsgName, MsgBin) when is_list(MsgName) andalso is_binary(MsgBin) ->
  msg_pb:decode_msg(MsgBin, list_to_atom2(MsgName)).

decode_reply_msg(MsgBin) when is_binary(MsgBin) ->
  #'xgame.reply_message'{msgName = MsgName, payload = Payload, error = Error} = rpc_pb:decode_msg(MsgBin, 'xgame.reply_message'),
  case Error of
    Null when Null == undefined orelse Null == "" ->
      {ok, decode_msg_by_name(MsgName, Payload)};
    _ ->
      list_to_atom2(Error)
  end.

encode_mfa(Module, Function, Args) ->
  ok = msg_pb:verify_msg(Args),
  Msg = #'xgame.pb_mfa'{module = atom_to_list2(Module), function = atom_to_list2(Function), args = #'xgame.rpc_params'{msgName = atom_to_list2(element(1, Args)), payload = msg_pb:encode_msg(Args)}},
  ReqMsg = #'xgame.req_message'{node_msg = Msg},
  {ok, rpc_pb:encode_msg(ReqMsg)}.

decode_mfa(MsgBin) ->
  #'xgame.req_message'{node_msg = #'xgame.pb_mfa'{module = Module, function = Func, args = #'xgame.rpc_params'{msgName = MsgName, payload = Payload}}} = rpc_pb:decode_msg(MsgBin, 'xgame.req_message'),
  {ok, list_to_atom(Module), list_to_atom(Func), decode_msg_by_name(MsgName, Payload)}.

encode_process_req(Target, From, PbMsg) ->
  ok = msg_pb:verify_msg(PbMsg),
  RpcParams = #'xgame.rpc_params'{msgName = atom_to_list(element(1, PbMsg)), payload = msg_pb:encode_msg(PbMsg)},
  {ok, TargetPidBin} = xrpc_util:encode_pid(Target),
  {ok, SourcePidBin} = xrpc_util:encode_pid(From),
  ReqMsg = #'xgame.req_message'{process_msg = #'xgame.process_msg'{target = TargetPidBin, source = SourcePidBin, params = RpcParams}},
  {ok, rpc_pb:encode_msg(ReqMsg)}.

decode_process_req(BinData) when is_binary(BinData) ->
  #'xgame.req_message'{process_msg = #'xgame.process_msg'{target = Target, source = Source, params = #'xgame.rpc_params'{msgName = MsgName, payload = Payload}}} = rpc_pb:decode_msg(BinData, 'xgame.req_message'),
  {ok, TargetPid} = xrpc_util:decode_pid(Target),
  {ok, FromPid} = xrpc_util:decode_pid(Source),
  {ok, TargetPid, FromPid, decode_msg_by_name(MsgName, Payload)}.

decode_packet(<<Flag:16/?UNSIGNINT, Seq:32/?UNSIGNINT, BinData/binary>>) ->
  {ok, Flag, Seq, BinData}.
encode_packet(Flag, Seq, BinData) ->
  {ok, <<Flag:16/?UNSIGNINT, Seq:32/?UNSIGNINT, BinData/binary>>}.

atom_to_list2(List) when is_list(List) ->
  List;
atom_to_list2(Atom) when is_atom(Atom) ->
  atom_to_list(Atom).
list_to_atom2(Atom) when is_atom(Atom) ->
  Atom;
list_to_atom2(List) when is_list(List) ->
  list_to_atom(List).

