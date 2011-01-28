%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_server).
-include_lib("agner.hrl").
-include_lib("typespecs/include/typespecs.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 


-record(state, {
          pushed_at
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
       pushed_at = ets:new(agner_pushed_at,[public])
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
-type agner_call_spec() :: {spec, agner_spec_name(), agner_spec_version()}.
-type agner_call_spec_url() :: {spec_url, agner_spec_name(), agner_spec_version()}.
-type agner_call_index() :: index.
-type agner_call_fetch() :: {fetch, agner_spec_name(), agner_spec_version(), directory()}.
-type agner_call_versions() :: {versions, agner_spec_name()}.
-type agner_internal_call_pushed_at_updates() :: {pushed_at_updates, list({agner_spec_name(), string()})}.
-type agner_internal_call_pushed_at() :: {pushed_at, agner_spec_name()}.

-spec handle_call(agner_call_spec(), gen_server_from(), gen_server_state()) -> gen_server_async_reply(agner_spec()|{error, bad_version}) ;
                 (agner_call_spec_url(), gen_server_from(), gen_server_state()) -> gen_server_async_reply(url()|{error, bad_version}) ;
                 (agner_call_index(), gen_server_from(), gen_server_state()) -> gen_server_async_reply(list(agner_spec_name())) ;
                 (agner_call_fetch(), gen_server_from(), gen_server_state()) -> gen_server_async_reply(ok | {error, any()}) ;
                 (agner_call_versions(), gen_server_from(), gen_server_state()) -> gen_server_async_reply(list(agner_spec_version()) | not_found_error());
                 (agner_internal_call_pushed_at_updates(), gen_server_from(), gen_server_state()) -> gen_server_async_reply(ok) ;
                 (agner_internal_call_pushed_at(), gen_server_from(), gen_server_state()) -> gen_server_async_reply(string() | undefined).

						 
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

handle_call({fetch, Name, Version, Directory}, From, #state{}=State) ->
	spawn_link(fun () ->
					   handle_fetch(Name, Version, Directory, From)
			   end),
	{noreply, State};

handle_call({versions, Name}, From, #state{}=State) ->
	spawn_link(fun () ->
					   handle_versions(Name, From, indices())
			   end),
	{noreply, State};

%% INTERNAL CALLS
handle_call({pushed_at_updates, Updates}, From, #state{ pushed_at = PushedAt }=State) ->
	spawn_link(fun () ->
                       ets:insert(PushedAt, Updates),
                       gen_server:reply(From, ok)
			   end),
	{noreply, State};

handle_call({pushed_at, Name}, From, #state{ pushed_at = PushedAt }=State) ->
	spawn_link(fun () ->
                       case ets:lookup(PushedAt, Name) of
                           [{Name, At}] ->
                               gen_server:reply(From, binary_to_list(At));
                           _ ->
                               gen_server:reply(From, undefined)
                       end
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
-spec handle_spec(agner_spec_name(), agner_spec_version(), gen_server_from(), agner_indices()) -> any().
handle_spec(_,_,From,[]) ->
	gen_server:reply(From, {error, not_found});
handle_spec(Name, Version, From, [Mod0|Rest]) ->
	Mod = index_module(Mod0),
    case sha1(Mod, Name, Version) of
        SHA1 when is_list(SHA1) ->
            case Mod:spec(Name, SHA1) of
                {error, not_found} ->
                    handle_spec(Name, Version, From, Rest);
                Data ->
                    gen_server:reply(From, Data)
            end;
        _ ->
            gen_server:reply(From, {error, bad_version})
    end.

-spec handle_spec_url(agner_spec_name(), agner_spec_version(), gen_server_from(), agner_indices()) -> any().
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

-spec handle_index(gen_server_from(), list(agner_spec_name()), list(tuple())) -> any().
handle_index(From, Acc, []) ->
    Repos = lists:reverse(Acc),
    RepoNames = lists:map(fun ({Name, _}) -> Name end, Repos),
    gen_server:call(?SERVER, {pushed_at_updates, Repos}),
	gen_server:reply(From, RepoNames);
handle_index(From, Acc, [Mod0|Rest]) ->
	Mod = index_module(Mod0),
	case Mod:repositories() of
		{error, not_found} ->
			handle_index(From, Acc, Rest);
		Repos ->
            handle_index(From, lists:map(fun (Repo) -> indexize(Mod0, Repo) end, Repos) ++ Acc, Rest)
	end.

-spec handle_fetch(agner_spec_name(), agner_spec_version(), directory(), gen_server_from()) -> any().
handle_fetch(Name, Version, Directory, From) ->
    case agner:spec(Name, Version) of
        {error, _} = Error ->
            gen_server:reply(From, Error);
        Spec ->
            URL = proplists:get_value(url, Spec),
            agner_download:fetch(URL, Directory),
            gen_server:reply(From, ok)
    end.

-spec handle_versions(agner_spec_name(), gen_server_from(), agner_indices()) -> any().
handle_versions(_,From,[]) ->
	gen_server:reply(From, {error, not_found});
handle_versions(Name, From, [Mod0|Rest]) ->
	Mod = index_module(Mod0),
	case Mod:repository(Name) of
		{error, not_found} ->
			handle_versions(Name, From, Rest);
		_ ->
            Branches = lists:map(fun({Branch, _}) -> {branch, Branch} end,
                                  Mod:branches(Name)),
            Tags = lists:map(fun({Tag, _}) -> {tag, Tag} end,
                                  Mod:tags(Name)),
            gen_server:reply(From, Branches ++ Tags)
	end.

-spec sha1(agner_index(), agner_spec_name(), agner_spec_version()) -> sha1().
                  
sha1(Mod, Name, Version) ->
    case Version of
        {branch, Branch} ->
            Branch;
        {tag, Tag} ->
            Tags = Mod:tags(Name),
            proplists:get_value(Tag, Tags);
        no_such_version ->
            no_such_version
    end.


index_module({github, Account}) ->
	{agner_github, Account}.

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
