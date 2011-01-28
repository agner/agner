%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    agner_sup:start_link().

stop(_State) ->
    ok.
