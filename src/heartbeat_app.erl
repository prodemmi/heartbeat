%%%-------------------------------------------------------------------
%% @doc heartbeat public API
%% @end
%%%-------------------------------------------------------------------

-module(heartbeat_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lager:start(),
    ok = heartbeat_store:init(),
    heartbeat_sup:start_link().

stop(_State) ->
    ok.
