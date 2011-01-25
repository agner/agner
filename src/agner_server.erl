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


-record(state, {}).

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
	{ok, #state{}}.

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
-type agner_call_index() :: index.

-spec handle_call(agner_call_spec(), gen_server_from(), gen_server_state()) -> gen_server_async_reply(agner_spec()) ;
                 (agner_call_index(), gen_server_from(), gen_server_state()) -> gen_server_async_reply(list(agner_spec_name())).
						 
handle_call({spec, Name, Version}, From, #state{}=State) ->
	spawn_link(fun () ->
					   handle_spec(Name, Version, From, indices())
			   end),
	{noreply, State};

handle_call(index, From, #state{}=State) ->
	spawn_link(fun () ->
					   handle_index(From, indices())
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
-spec handle_spec(agner_spec_name(), agner_spec_version(), gen_server_from(), list(tuple())) -> any().
handle_spec(_,_,From,[]) ->
	gen_server:reply(From, {error, not_found});
handle_spec(Name, Version, From, [Mod0|Rest]) ->
	Mod = index_module(Mod0),
	case Mod:repository(Name) of
		{error, not_found} ->
			handle_spec(Name, Version, From, Rest);
		_ ->
			SHA1 = 
				case Version of
					master ->
						Branches = Mod:branches(Name),
						proplists:get_value("master", Branches);
					_ ->
						Tags = Mod:tags(Name),
						proplists:get_value(Version, Tags)
				end,
			Data = Mod:spec(Name, SHA1),
			gen_server:reply(From, Data)
	end.

-spec handle_index(gen_server_from(), list(tuple())) -> any().
handle_index(From,[]) ->
	gen_server:reply(From, {error, not_found});
handle_index(From, [Mod0|Rest]) ->
	Mod = index_module(Mod0),
	case Mod:repositories() of
		{error, not_found} ->
			handle_index(From, Rest);
		Repos ->
			gen_server:reply(From, Repos)
	end.


index_module({github, Account}) ->
	{agner_github, Account}.

indices() ->
    case application:get_env(indices) of
        {ok, Val} ->
            Val;
        undefined ->
            []
    end.
