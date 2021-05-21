%%%-------------------------------------------------------------------
%% @doc sopost_blackjack public API
%% @end
%%%-------------------------------------------------------------------

-module(sopost_blackjack_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sopost_blackjack_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
