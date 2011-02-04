%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(SUP(I, Timeout), {I, {I, start_link, []}, permanent, Timeout, supervisor, [I]}).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_all, 5, 10}, [
                                  ?SUP(agner_repo_server_sup, infinity),
								  ?CHILD(agner_server, worker)
								 ]} }.

