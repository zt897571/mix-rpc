%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_compile version 4.20.0

-ifndef(msg_pb).
-define(msg_pb, true).

-define(msg_pb_gpb_version, "4.20.0").


-ifndef('XGAME.TEST_MSG_PB_H').
-define('XGAME.TEST_MSG_PB_H', true).
-record('xgame.test_msg',
        {msg = []               :: unicode:chardata() | undefined, % = 1, optional
         delayTime = 0          :: integer() | undefined, % = 2, optional, 32 bits
         rand = 0               :: integer() | undefined, % = 3, optional, 32 bits
         testBt = <<>>          :: iodata() | undefined % = 4, optional
        }).
-endif.

-ifndef('XGAME.REQGETPIDLIST_PB_H').
-define('XGAME.REQGETPIDLIST_PB_H', true).
-record('xgame.ReqGetPidList',
        {
        }).
-endif.

-ifndef('XGAME.REPLYGETPIDLIST_PB_H').
-define('XGAME.REPLYGETPIDLIST_PB_H', true).
-record('xgame.ReplyGetPidList',
        {pids = []              :: [iodata()] | undefined % = 1, repeated
        }).
-endif.

-endif.
