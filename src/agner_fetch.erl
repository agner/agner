%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_fetch).

-behaviour(gen_fsm2).
-include_lib("kernel/include/file.hrl").

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, 
         handle_state/2,
         ready/2, fetchable/2, fetched/2, buildable/2, installable/2,
         handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(opts_rec, {
          package,
          directory,
          version,
          build,
          addpath,
          install,
          spec,
          package_path,
          quiet
         }).

-record(state, {
          opts = #opts_rec{},
          repo_dir,
          fetched_steps = [check_requirements, fetch_requirements, caveats],
          build_steps = [rebar, build_command, add_path],
          install_steps = [install_command, print_prefix]
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Opts) ->
    gen_fsm2:start_link(?MODULE, Opts, []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm2:start/[3,4] or
%% gen_fsm2:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init(Opts) ->
    {ok, ready, #state{
           opts = opts_to_rec(Opts)
          }}.


%% Stop if no package name is specified
handle_state(ready,  #state{ opts = #opts_rec{ package = undefined }} = State) ->
    {stop, {error, {package_missing, "Package name required"}}, State};

%% Stop if no package version can satisfy given criteria
handle_state(ready, #state{ opts = #opts_rec{ version = undefined }} = State) ->
    {stop, {error, {no_version, "No version that satisfy given criteria"}}, State};

%% Check atleast: version
handle_state(ready, #state{ opts = #opts_rec{ version = ("atleast:" ++ VersionCond) = Version, package = Package } = Opts 
                          } = State) when is_list(VersionCond) ->
    CheckedVersion = agner_spec:version_to_list(agner_spec:list_to_version(Package, Version)),
    handle_state(ready, State#state{ opts = Opts#opts_rec{ version = CheckedVersion } });

%% By default, if directory is not specified, it should assume its value from package name
handle_state(ready, #state{ opts = #opts_rec{ version = Version, directory = undefined, 
                                              package = Package } = Opts} = State) when is_list(Version) ->
    handle_state(ready, State#state{ opts = Opts#opts_rec { directory = Package } });

%% Everything is ready to go
handle_state(ready, #state{  opts = #opts_rec{ version = Version, directory = Directory0 } = Opts
                          } = State) when is_list(Version) ->
    Directory = filename:absname(Directory0),
    gen_fsm:send_event(self(), next),
    {ok, State#state{ opts = Opts#opts_rec{ directory = Directory } }};

%% If specification was not found, stop
handle_state(fetchable, #state{ opts = #opts_rec{ spec = {spec, {error, not_found}} }} = State) ->
    {stop, {error, {package_not_found, "Package not found"}}, State};

%% Retrieve specification from the index
handle_state(fetchable, #state{ opts = #opts_rec{ spec = undefined, package = Package, version = Version } = Opts} = State) ->
    Spec0 = agner:spec(Package, Version),
    {ok, RepoServer} = agner_repo_server:create(Package, agner_spec:list_to_version(Package, Version)),
    handle_state(fetchable, State#state{ opts = Opts#opts_rec{ spec = {spec, Spec0} }, repo_dir = agner_repo_server:file(RepoServer,"")});

%% If no --package-path is specified, assume absolute path to current directory (important only in conjunction with
%% --spec)
handle_state(fetchable, #state{ opts = #opts_rec{ spec = Spec, package_path = undefined } = Opts
                              } = State) when is_list(Spec) ->
    handle_state(fetchable, State#state{  opts = Opts#opts_rec{ package_path = filename:absname(".") }});

%% Read specification if supplied with --spec
handle_state(fetchable, #state{ opts = #opts_rec{ spec = Spec, package_path = PackagePath } = Opts
                              } = State) when is_list(Spec) andalso is_list(PackagePath) ->
    {ok, Spec0} = agner_spec:normalize(file:consult(Spec)),
    handle_state(fetchable, State#state{ opts = Opts#opts_rec{ spec = {spec, Spec0} }, repo_dir = PackagePath } );

%% Everything is ready to go, fetch
handle_state(fetchable, #state{ opts = #opts_rec{spec = {spec, Spec}, version = Version, directory = Directory }
                              } = State) when is_list(Directory) andalso is_list(Version) ->
    agner:fetch(Spec, Version, Directory),
    gen_fsm2:send_event(self(), next),
    {ok, State};

%% Execute steps in `fetched` until there is nothing else to do
handle_state(fetched, #state{ fetched_steps = []  } = State) ->
    gen_fsm2:send_event(self(), next),
    {ok, State};

handle_state(fetched, #state{ fetched_steps = [Step|Rest]  } = State) ->
    gen_fsm2:send_event(self(), Step),
    {ok, State#state{ fetched_steps = Rest } };

%% Execute steps in `buildable` until there is nothing else to do
handle_state(buildable, #state{ build_steps = []  } = State) ->
    gen_fsm2:send_event(self(), next),
    {ok, State};

handle_state(buildable, #state{ build_steps = [Step|Rest]  } = State) ->
    gen_fsm2:send_event(self(), Step),
    {ok, State#state{ build_steps = Rest } };

%% Execute steps in `installable` until there is nothing else to do
handle_state(installable, #state{ install_steps = []  } = State) ->
    gen_fsm2:send_event(self(), next),
    {ok, State};

handle_state(installable, #state{ install_steps = [Step|Rest]  } = State) ->
    gen_fsm2:send_event(self(), Step),
    {ok, State#state{ install_steps = Rest } }.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm2:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------

ready(next, State) ->
    {next_state, fetchable, State}.

fetchable(next, State) ->
    {next_state, fetched, State}.


fetched(check_requirements, #state{  opts = #opts_rec{ spec = {spec, Spec} } } = State) ->
    case unsatisfied_requirements(Spec) of
        [] -> %% all requirements are satisfied
            {next_state, fetched, State};
        Requirements ->
            Errors =  lists:map(fun ({Package, Version}) ->
                                      {version_mistmatch, 
                                       "Your " ++ Package ++ " version is mismatched (" ++
                                           current_agner_version() ++ ", " ++ Version ++ " required)"}
                              end, Requirements),
            {stop, {error, Errors}, State}
    end;

fetched(fetch_requirements, #state{ opts = #opts_rec{ spec = {spec, Spec} } = Opts } = State) ->
    Requirements = package_requirements(Spec),
    lists:foreach(fun ({ReqName, ReqVersion}) ->
                          build_dep(ReqName, ReqVersion, Opts);
                      (ReqName) ->
                          build_dep(ReqName, "@master", Opts)
                  end, Requirements),
    {next_state, fetched, State};

fetched(caveats, #state{ opts = #opts_rec{spec = {spec, Spec} }} = State) ->
    case proplists:get_value(caveats, Spec) of
        undefined ->
            ignore;
        Caveats when is_list(Caveats) ->
            io:format("=== CAVEATS ===~n~n~s~n~n",[Caveats])
    end,
    {next_state, fetched, State};

fetched(next, State) ->
    {next_state, buildable, State}.


buildable(next, State) ->
    {next_state, installable, State};

buildable(_, #state{ opts = #opts_rec{ build = false }} = State) ->
    {next_state, buildable, State};

buildable(rebar, #state{ opts = #opts_rec{ build = true } = Opts} = State) ->
    rebar(Opts),
    {next_state, buildable, State};

buildable(build_command, #state{ opts = #opts_rec{ build = true } = Opts, repo_dir = RepoDir} = State) ->
    os:putenv("AGNER_PACKAGE_REPO", RepoDir),
    case build_command(Opts) of
        ok ->
            {next_state, buildable, State};
        _ ->
            {stop, {error, {build_failed, "Build failed"}}, State}
    end;

buildable(add_path, #state{ opts = #opts_rec{ build = true } = Opts} = State) ->
    add_path(Opts),
    {next_state, buildable, State}.

installable(next, State) ->
    {stop, shutdown, State};

installable(_, #state{ opts = #opts_rec{ install = false }} = State) ->
    {next_state, installable, State};

installable(install_command, #state{ opts = #opts_rec{ install = true } = Opts, repo_dir = RepoDir} = State) ->
    os:putenv("AGNER_PACKAGE_REPO", RepoDir),
    case install_command(Opts) of
        ok ->
            {next_state, installable, State};
        _ ->
            {stop, {error, {install_failed, "Installation failed"}}, State}
    end;

installable(print_prefix, #state{ opts = #opts_rec{ install = true, package = Package, version = Version } } = State) ->
    io:format("Installed to:~n"),
    agner_main:handle_command(prefix,[{package, Package}, {version, Version}]),
    {next_state, installable, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm2:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
%% state_name(_Event, _From, State) ->
%%     Reply = ok,
%%     {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm2:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm2:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
rec_to_opts(Rec) ->
    Fields = record_info(fields, opts_rec),
    {_, Result} = lists:foldl(fun (Field, {I, L}) ->
                                      {I+1, [{Field, element(I+2, Rec)}|L]}
                              end, {0, []}, Fields),
    Result.

opts_to_rec(Opts) ->
    Fields = record_info(fields, opts_rec),
    list_to_tuple(lists:reverse(lists:foldl(fun (Field, StateAcc) ->
                                                    [proplists:get_value(Field, Opts)|StateAcc]
                                            end, [opts_rec], Fields))).

requires(Spec) ->
    lists:sort(fun ({"agner", _},_) ->
                       true;
                   ("agner", _) ->
                       true;
                   (A,B) ->
                       A =< B
               end, proplists:get_value(requires, Spec)).

deps_dir(Spec, Directory) ->
    filename:join(Directory, proplists:get_value(deps_dir, Spec)).

unsatisfied_requirements(Spec) ->
    lists:filter(fun ("agner") ->
                         false;
                     ({"agner", "atleast:" ++ AgnerVersion}) ->
                         agner_spec:version_compare('>', AgnerVersion, current_agner_version());
                     ({"agner", AgnerVersion}) ->
                         AgnerVersion /= current_agner_version();
                     (_) ->
                         false
                 end, requires(Spec)).

package_requirements(Spec) ->
    lists:filter(fun ("agner") ->
                         false;
                     ({"agner", _}) ->
                         false;
                     (_) ->
                         true
                 end, requires(Spec)).

current_agner_version() ->
    {agner,_,CurrentAgnerVersion} = lists:keyfind(agner,1,application:which_applications()),
    CurrentAgnerVersion.

build_dep(ReqName, ReqVersion, #opts_rec{ spec = {spec, Spec}, directory = Directory } = Opts) ->
    io:format("[Building dependency: ~s]~n", [ReqName]),
    agner_main:handle_command(fetch, [{package, ReqName},{version, ReqVersion},
                                      {directory, filename:join(deps_dir(Spec, Directory),ReqName)}|
                                      proplists:delete(spec,rec_to_opts(Opts))]).

rebar(#opts_rec{ spec = {spec, Spec}, directory = Directory }) ->
    case proplists:get_value(rebar_compatible, Spec) of
        true ->
            io:format("[Building...]~n"),
            {ok, Cwd} = file:get_cwd(),
            file:set_cwd(Directory),
            rebar_config:set_global(shutdown_agner, false), %% prevents rebar from shutting down agner
            RebarCommands = proplists:get_value(rebar_commands, Spec),
            rebar:main(RebarCommands),
            file:set_cwd(Cwd);
        _ ->
            ignore
    end.

build_command(#opts_rec{ spec = {spec, Spec}, directory = Directory, quiet = Quiet, package = Package, version = Version } = Opts) ->
    os:putenv("AGNER_PACKAGE_NAME", Package),
    os:putenv("AGNER_PACKAGE_VERSION", Version),

    case proplists:get_value(build_command, Spec) of
        undefined ->
            case proplists:get_value(rebar_compatible, Spec) of
                false ->
                    io:format("WARNING: No build_command specified, can't build this package~n");
                _ ->
                    ok
            end;
        Command ->
            set_install_prefix(Opts),
            io:format("[Building...]~n"),
            Port = open_port({spawn,"sh -c \"" ++ Command ++ "\""},[{cd, Directory},exit_status,stderr_to_stdout,use_stdio, stream]),
            unlink(Port),
            PortHandler = fun (F) ->
                                  receive
                                      {'EXIT', Port, normal} ->
                                          ok;
                                      {'EXIT', Port, _} ->
                                          error;
                                      {Port,{exit_status,0}} ->
                                          ok;
                                      {Port,{exit_status,_}} ->
                                          error;
                                      {Port, {data, D}} when not Quiet andalso is_list(D) ->
                                          io:format("~s",[D]),
                                          F(F);
                                      _ ->
                                          F(F)
                                  end
                          end,
            Result = PortHandler(PortHandler),
            receive
                {'EXIT', Port, normal} -> %% flush port exit
                    ok
            after 0 ->
                    ok
            end,
            Result
    end.

add_path(#opts_rec{ directory = Directory, package = Package, addpath = true }) ->
    {ok, F} = file:open(filename:join(os:getenv("HOME"),".erlang"),
                        [append]),
    file:write(F, io_lib:format("code:add_patha(\"~s/ebin\"). %% {agner, ~s}~n", [Directory, Package])),
    file:close(F);

add_path(#opts_rec{ addpath = false }) ->
    ignore.
    

install_command(#opts_rec{ spec = {spec, Spec}, directory = Directory, quiet = Quiet, package = Package, version = Version } = Opts) ->
    os:putenv("AGNER_PACKAGE_NAME", Package),
    os:putenv("AGNER_PACKAGE_VERSION", Version),

    filelib:ensure_dir(filename:join([os:getenv("AGNER_PREFIX"),"packages"]) ++ "/"),
    InstallPrefix = set_install_prefix(Opts),
    os:cmd("rm -rf " ++ InstallPrefix),
    ok = filelib:ensure_dir(InstallPrefix ++ "/"),
    case proplists:get_value(install_command, Spec) of
        undefined ->
            io:format("ERROR: No install_command specified, can't install this package~n");
        Command ->
            io:format("[Installing...]~n"),
            Port = open_port({spawn,"sh -c \"" ++ Command ++ "\""},[{cd, Directory},exit_status,stderr_to_stdout,use_stdio, stream]),
            PortHandler = fun (F) ->
                                  receive
                                      {'EXIT', Port, normal} ->
                                          ok;
                                      {'EXIT', Port, _} ->
                                          error;
                                      {Port,{exit_status,0}} ->
                                          ok;
                                      {Port,{exit_status,_}} ->
                                          error;
                                      {Port, {data, D}} when not Quiet andalso is_list(D) ->
                                          io:format("~s",[D]),
                                          F(F);
                                      _ ->
                                          F(F)
                                  end
                          end,
            Result = PortHandler(PortHandler),
            receive
                {'EXIT', Port, normal} -> %% flush port exit
                    ok
            after 0 ->
                    ok
            end,
            case Result of
                ok ->
                    case proplists:get_value(bin_files, Spec) of
                        undefined ->
                            ignore;
                        Files ->
                            lists:foreach(fun (File) ->
                                                  Symlink = filename:join(os:getenv("AGNER_BIN"),filename:basename(File)),
                                                  File1 = filename:join([InstallPrefix,File]),
                                                  file:delete(Symlink),
                                                  {ok, #file_info{mode = Mode}} = file:read_file_info(File1),
                                                  file:change_mode(File1, Mode bor 8#00011),
                                                  ok = file:make_symlink(File1, Symlink)
                                          end, Files)
                    end,
                    ok;
                _ ->
                    Result
            end
    end.

set_install_prefix(#opts_rec{ package = Package, version = Version }) ->
    InstallPrefix = filename:join([os:getenv("AGNER_PREFIX"),"packages",Package ++ "-" ++ Version]),
    os:putenv("AGNER_INSTALL_PREFIX", InstallPrefix),
    InstallPrefix.
