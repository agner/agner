%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_server).
-include_lib("agner.hrl").
-include_lib("typespecs/include/typespecs.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([spec/2, spec_url/2, index/0, fetch/3, versions/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


-record(state, {
         }).

-type gen_server_state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Ask the server for a spec on Name and Version
%% @end
-spec spec(agner_package_name(), agner_package_version()) -> agner_spec().
                  
spec(Name, Version) ->
    gen_server:call(?SERVER, {spec, Name, Version}).

%% @doc Ask the server for a spec URL
%% @end
-spec spec_url(agner_package_name(), agner_package_version()) -> url().

spec_url(Name, Version) ->
    gen_server:call(?SERVER, {spec_url, Name, Version}).

%% @doc Ask the server for an index
%% @end
-spec index() -> list(agner_package_name()).
                   
index() ->
    gen_server:call(?SERVER, index).

%% @doc Fetch a package/project to a directory
%% @end
-spec fetch(agner_package_name(), agner_package_version(), directory()) -> ok | not_found_error();
           (agner_spec(), any(), directory()) -> ok | not_found_error().
                   
fetch(NameOrSpec, Version, Directory) ->
        gen_server:call(?SERVER, {fetch, NameOrSpec, Version, Directory}, infinity).

%% @doc Ask for the versions of a given package, Name
%% @end
-spec versions(agner_package_name()) -> list(agner_package_version()).
                      
versions(Name) ->
    gen_server:call(?SERVER, {versions, Name}).

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
-spec init([]) -> gen_server_init_result().

init([]) ->
	{ok, #state{
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
-type agner_call_spec() :: {spec, agner_package_name(), agner_package_version()}.
-type agner_call_spec_url() :: {spec_url, agner_package_name(), agner_package_version()}.
-type agner_call_index() :: index.
-type agner_call_fetch() :: {fetch, agner_package_name() | agner_spec(), agner_package_version(), directory()}.
-type agner_call_versions() :: {versions, agner_package_name()}.

-spec handle_call(agner_call_spec(), gen_server_from(), gen_server_state()) -> gen_server_async_reply(agner_spec()|{error, bad_version}) ;
                 (agner_call_spec_url(), gen_server_from(), gen_server_state()) -> gen_server_async_reply(url()|{error, bad_version}) ;
                 (agner_call_index(), gen_server_from(), gen_server_state()) -> gen_server_async_reply(list(agner_package_name())) ;
                 (agner_call_fetch(), gen_server_from(), gen_server_state()) -> gen_server_async_reply(ok | {error, any()}) ;
                 (agner_call_versions(), gen_server_from(), gen_server_state()) -> gen_server_async_reply(list(agner_package_version()) | not_found_error()).

handle_call({spec, Name, Version}, From, #state{}=State) ->
	spawn_link(fun () ->
					   handle_spec(Name, Version, From, indices())
			   end),
	{noreply, State};

handle_call({spec_url, Name, Version}, From, #state{}=State) ->
	spawn_link(fun () ->
					   handle_spec_url(Name, Version, From, indices())
			   end),
	{noreply, State};

handle_call(index, From, #state{}=State) ->
	spawn_link(fun () ->
					   handle_index(From, [], indices())
			   end),
	{noreply, State};

handle_call({fetch, NameOrSpec, Version, Directory}, From, #state{}=State) ->
	spawn_link(fun () ->
					   handle_fetch(NameOrSpec, Version, Directory, From)
			   end),
	{noreply, State};

handle_call({versions, Name}, From, #state{}=State) ->
	spawn_link(fun () ->
					   handle_versions(Name, From, indices())
			   end),
	{noreply, State}.


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
handle_cast(_Msg, State) ->
	{noreply, State}.

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

-spec handle_info(any(), gen_server_state()) ->
						 gen_server_handle_info_result().

handle_info(_, State) ->
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
-spec handle_spec(agner_package_name(), agner_package_version(), gen_server_from(), agner_indices()) -> any().
handle_spec(_,_,From,[]) ->
	gen_server:reply(From, {error, not_found});
handle_spec(Name, Version, From, [Mod0|Rest]) ->
	Mod = index_module(Mod0),
    case Mod:spec(Name, Version) of
        {error, not_found} ->
            handle_spec(Name, Version, From, Rest);
        Data ->
            gen_server:reply(From, Data)
    end.

-spec handle_spec_url(agner_package_name(), agner_package_version(), gen_server_from(), agner_indices()) -> any().
handle_spec_url(_,_,From,[]) ->
	gen_server:reply(From, {error, not_found});
handle_spec_url(Name, Version, From, [Mod0|Rest]) ->
	Mod = index_module(Mod0),
    case sha1(Mod, Name, Version) of
        SHA1 when is_list(SHA1) ->
            case Mod:spec_url(Name, SHA1) of
                {error, not_found} ->
                    handle_spec_url(Name, Version, From, Rest);
                URL ->
                    gen_server:reply(From, URL)
            end;
        _ ->
            gen_server:reply(From, {error, bad_version})
    end.

-spec handle_index(gen_server_from(), list(agner_package_name()), list(tuple())) -> any().
handle_index(From, Acc, []) ->
    Repos = lists:reverse(Acc),
    RepoNames = lists:map(fun ({Name, _}) -> Name end, Repos),
    lists:foreach(fun (Name) ->
                          {ok, Pid} = agner_repo_server:create(Name, {flavour, "master"}),
                          agner_repo_server:set_pushed_at(Pid, binary_to_list(proplists:get_value(Name, Repos)))
                  end, RepoNames),
	gen_server:reply(From, RepoNames);
handle_index(From, Acc, [Mod0|Rest]) ->
	Mod = index_module(Mod0),
	case Mod:repositories() of
		{error, not_found} ->
			handle_index(From, Acc, Rest);
		Repos ->
            handle_index(From, lists:map(fun (Repo) -> indexize(Mod0, Repo) end, Repos) ++ Acc, Rest)
	end.

-spec handle_fetch(agner_package_name() | agner_spec(), agner_package_version(), directory(), gen_server_from()) -> any().
handle_fetch(NameOrSpec, Version, Directory, From) ->
    case io_lib:printable_list(NameOrSpec) of
        true ->
            case agner:spec(NameOrSpec, Version) of
                {error, _} = Error ->
                    gen_server:reply(From, Error);
                Spec ->
                    agner_download:fetch(Spec, Directory),
                    gen_server:reply(From, ok)
            end;
        false -> %% it is a spec
            agner_download:fetch(NameOrSpec, Directory),
            gen_server:reply(From, ok)
    end.

-spec handle_versions(agner_package_name(), gen_server_from(), agner_indices()) -> any().
handle_versions(_,From,[]) ->
	gen_server:reply(From, {error, not_found});
handle_versions(Name, From, [Mod0|Rest]) ->
	Mod = index_module(Mod0),
	case Mod:repository(Name) of
		{error, not_found} ->
			handle_versions(Name, From, Rest);
		_ ->
            Branches = lists:map(fun
                                     ({[$%|_],_}) -> undefined;
                                     ({"gh-pages",_}) -> undefined;
                                     ({Branch, _}) -> {flavour, Branch} end,
                                  Mod:branches(Name)),
            Tags = lists:map(fun ({[$%|_],_}) -> undefined;
                                 ({Tag, _}) -> {release, Tag} end,
                                  Mod:tags(Name)),
            gen_server:reply(From, lists:filter(fun (undefined) -> false; (_) -> true end, Branches ++ Tags))
	end.

-spec sha1(agner_index(), agner_package_name(), agner_package_version()) -> sha1().
                  
sha1(Mod, Name, Version) ->
    case Version of
        {flavour, Branch} ->
            Branch;
        {release, Tag} ->
            Tags = Mod:tags(Name),
            proplists:get_value(Tag, Tags);
        no_such_version ->
            no_such_version
    end.


index_module(T) ->
    case application:get_env(index_modules) of
        {ok, Modules} ->
            setelement(1,T,proplists:get_value(element(1,T), Modules));
        _ ->
            T
    end.

indexize({github, "agner"}, Name) ->
    Name;
indexize({github, Account}, Name) ->
    Account ++ "/" ++ Name.

indices() ->
    case application:get_env(indices) of
        {ok, Val} ->
            Val;
        undefined ->
            []
    end.
