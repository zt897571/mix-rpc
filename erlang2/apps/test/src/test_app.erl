%%%-------------------------------------------------------------------
%% @doc test public API
%% @end
%%%-------------------------------------------------------------------

-module(test_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  xrpc:start(),
  test:start(),
  test_sup:start_link().

stop(_State) ->
  ok.

%% internal functions
