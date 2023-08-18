%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_compile version 4.19.7

-ifndef(rpc_pb).
-define(rpc_pb, true).

-define(rpc_pb_gpb_version, "4.19.7").

-ifndef('REQ_MESSAGE_PB_H').
-define('REQ_MESSAGE_PB_H', true).
-record(req_message,
        {seq = 0                :: non_neg_integer() | undefined, % = 1, optional, 32 bits
         source = []            :: unicode:chardata() | undefined, % = 2, optional
         target = []            :: unicode:chardata() | undefined, % = 3, optional
         msgName = []           :: unicode:chardata() | undefined, % = 4, optional
         payload = <<>>         :: iodata() | undefined % = 5, optional
        }).
-endif.

-ifndef('REPLY_MESSAGE_PB_H').
-define('REPLY_MESSAGE_PB_H', true).
-record(reply_message,
        {seq = 0                :: non_neg_integer() | undefined, % = 1, optional, 32 bits
         target = []            :: unicode:chardata() | undefined, % = 2, optional
         msgName = []           :: unicode:chardata() | undefined, % = 3, optional
         payload = <<>>         :: iodata() | undefined, % = 4, optional
         err_code = []          :: unicode:chardata() | undefined % = 5, optional
        }).
-endif.

-endif.