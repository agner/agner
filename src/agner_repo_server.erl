-module(agner_repo_server).
-include_lib("typespecs/include/typespecs.hrl").
-include_lib("agner.hrl").
-behaviour(gen_server).

%% API
-export([create/2, start_link/2]).
-export([set_pushed_at/2, pushed_at/1, clone/2, file/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 


-type pushed_at() :: string() | undefined.

-record(state, {
          name :: agner_package_name() | undefined,
          version :: agner_package_version() | undefined,
          pushed_at :: pushed_at(),
          directory :: directory() | undefined
         }).

-type gen_server_state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec create(agner_package_name(), agner_package_version()) -> {ok, pid()}.

create(Name, Version) ->
    case gproc:lookup_local_name({?SERVER, Name, Version}) of
        Pid when is_pid(Pid) ->
            {ok, Pid};
        _ ->
            supervisor:start_child(agner_repo_server_sup, [Name, Version])
    end.
                    
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(agner_package_name(), agner_package_version()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(agner_package_name(), agner_package_version()) -> {ok, pid()} | ignore | {error, term()}.
                         
start_link(Name, Version) ->
    gen_server:start_link(?MODULE, {Name, Version}, []).

-spec set_pushed_at(pid(), pushed_at()) -> ok.

set_pushed_at(Pid, PushedAt) ->                           
    gen_server:cast(Pid, {set_pushed_at, PushedAt}).

-spec pushed_at(pid()) -> pushed_at().
                       
pushed_at(Pid) ->
    gen_server:call(Pid, pushed_at).

-type repo_url_function() :: fun((agner_package_name()) -> url()).

-spec clone(pid(), repo_url_function()) -> ok.

clone(Pid, Fun) ->
    gen_server:call(Pid, {clone, Fun}, infinity).

-spec file(pid(), file()) -> file() | not_found_error() | {error, not_cloned}.
                  
file(Pid, Filename) ->
    gen_server:call(Pid, {file, Filename}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init({agner_package_name(), agner_package_version()}) -> gen_server_init_result().
                  
init({Name, Version}) ->
    process_flag(trap_exit, true),
    gproc:add_local_name({?SERVER, Name, Version}),
    {ok, #state{
       name = Name,
       version = Version
      }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-type pushed_at_call() :: pushed_at().
-type clone_call() :: {clone, repo_url_function()}.
-type file_call() :: {file, file()}.

-spec handle_call(pushed_at_call(), gen_server_from(), gen_server_state()) ->
                         gen_server_handle_call_result(pushed_at());
                 (clone_call(), gen_server_from(), gen_server_state()) ->
                         gen_server_handle_call_result(ok);
                 (file_call(), gen_server_from(), gen_server_state()) ->
                         gen_server_handle_call_result(file() | not_found_error() | {error, not_cloned}).

handle_call(pushed_at, _From, #state{ pushed_at = PushedAt } = State) ->
    {reply, PushedAt, State};

handle_call({clone, Fun}, _From, #state{ directory = undefined, name = Name, version = Version } = State) ->
    Directory = test_server:temp_name("/tmp/agner"),
    RepoName = apply(Fun,[Name]),
    ClonePort = agner_download:git(["clone", "-q", RepoName, Directory]),
    Result = agner_download:process_port(ClonePort, 
                                         fun () ->
                                                 PortCheckout = agner_download:git(["checkout","-q",version_to_ref(Version)],
                                                                                   [{cd, Directory}]),
                                                 agner_download:process_port(PortCheckout, fun () ->
                                                                                                   ok
                                                                                           end)
                                         end),
    case Result of 
        ok ->
            {reply, ok, State#state{ directory = Directory }};
        _ ->
            {reply, Result, State}
    end;

handle_call({clone, _Fun}, _From, #state{} = State) -> %% already cloned
    {reply, ok, State};

handle_call({file, Filename}, _From, #state{ directory = Directory} = State) when is_list(Directory) -> 
    F = filename:join(Directory, Filename),
    case (filelib:is_dir(F) or filelib:is_file(F)) of
        true ->
            {reply, F, State};
        false ->
            {reply, {error, not_found}, State}
    end;

handle_call({file, _Filename}, _From, #state{ directory = undefined } = State) -> 
    {reply, {error, not_cloned}, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-type set_pushed_at_cast() :: {set_pushed_at, pushed_at()}.

-spec handle_cast(set_pushed_at_cast(), gen_server_state()) -> gen_server_handle_cast_result().
                         
handle_cast({set_pushed_at, PushedAt}, #state{}=State) ->
    {noreply, State#state{ pushed_at = PushedAt} }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{ directory = Directory }) when is_list(Directory) ->
    os:cmd("rm -rf " ++ Directory),
    ok;
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec version_to_ref(agner_package_version()) -> string().
                            
version_to_ref({flavour, Branch}) ->
    "origin/" ++ Branch;
version_to_ref({release, Tag}) ->
    Tag.

