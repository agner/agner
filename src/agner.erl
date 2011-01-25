-module(agner).
-include_lib("agner.hrl").
-export([start/0,stop/0]).
-export([main/1]).
%% API
-export([spec/1, spec/2]).

start() ->
	inets:start(),
	ssl:start(),
	{ok, _Pid} = inets:start(httpc,[{profile, agner}]),
	application:start(agner).

stop() ->
	application:stop(agner),
	inets:stop(),
	ssl:stop().

main(_) ->
	start(),
	stop().

%%%===================================================================
%%% API
%%%===================================================================
-spec spec(agner_spec_name()) -> agner_spec().
-spec spec(agner_spec_name(), agner_spec_version()) -> agner_spec().

spec(Name) ->
	spec(Name, master).

spec(Name, Version) when is_atom(Name) ->
	spec(atom_to_list(Name),Version);

spec(Name, Version) ->
	gen_server:call(agner_server, {spec, Name, Version}).
