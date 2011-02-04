%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, prep_stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = agner_sup:start_link(),
    {ok, Pid, []}.

%% The reason for the code below is that we have a simple_one_for_one supervisor
%% (agner_repo_server_sup) and according to supervisor's documentation:
%% "Important note on simple-one-for-one supervisors: The dynamically created child 
%%  processes of a simple-one-for-one supervisor are not explicitly killed, regardless of
%%  shutdown strategy, but are expected to terminate when the supervisor does (that is, when
%%  an exit signal from the parent process is received)."
%% Since agner_repo_server is actually trapping exits to run terminate/2 to clean up
%% after itself, some (sometimes may be all of them) of its instances *do* receive an EXIT
%% from the supervisor and get their terminate/2 callbacks called; but since the supervisor
%% is not explicitly killing them and not waiting for them to die, it means by the time
%% supervisor is dead, some of them *may have* went through terminate/2 and some may have not,
%% but since the top supervisor was wrapping up its business and dying as well, the application
%% master (as a group leader) was going through killing all remaining children regardless of whether
%% they finished their business or not (and by business we mean terminate/2 here), and thus
%% preventing their proper cleanup.

%% So what we are doing here is we're memorizing the list of children that are about to terminate
%% in prep_stop/1 and put them into the application state, and once top supervisor is dead and
%% stop/1 is called, we pick them up from the state, setup monitors on them and wait until every
%% one of them is DOWN (and that means the stuff is now cleaned up)

%% This would probably take a whole lot more time to figure out if Fred Hebert hadn't offered
%% his help to brainstorm this problem through. Merci pour l'aide!


prep_stop(_) ->
    lists:map(fun ({_,P,_,_}) -> P end, supervisor:which_children(agner_repo_server_sup)).
    
stop(State) ->
    stop_loop(lists:map(fun (P) -> monitor(process, P) end, State)),
    ok.

stop_loop([]) ->
    ok;
stop_loop([Ref|Rest]) ->
    receive 
        {'DOWN',Ref,process,_,_} ->
            stop_loop(Rest)
    end.
