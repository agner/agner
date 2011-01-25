-module(agner).
-include_lib("agner.hrl").
-export([start/0,stop/0]).
-export([main/1]).
%% API
-export([spec/1, spec/2, index/0]).

start() ->
	inets:start(),
	ssl:start(),
	{ok, _Pid} = inets:start(httpc,[{profile, agner}]),
	application:start(agner).

stop() ->
    error_logger:delete_report_handler(error_logger_tty_h),
	application:stop(agner),
	inets:stop(),
	ssl:stop().

main(["spec"|Args]) ->
    OptSpec = [
               {package, undefined, undefined, string, "Package name"}
              ],
	start(),
    {ok, {Opts, _}} = getopt:parse(OptSpec, Args),
    io:format("~p~n",[spec(proplists:get_value(package, Opts))]),
	stop();

main(["list"|Args]) ->
    OptSpec = [
              ],
	start(),
    {ok, {_Opts, _}} = getopt:parse(OptSpec, Args),
    io:format("~p~n",[index()]),
	stop().

%%%===================================================================
%%% API
%%%===================================================================
-spec spec(agner_spec_name()) -> agner_spec() | not_found_error().
-spec spec(agner_spec_name(), agner_spec_version()) -> agner_spec() | not_found_error().

spec(Name) ->
	spec(Name, master).

spec(Name, Version) when is_atom(Name) ->
	spec(atom_to_list(Name),Version);

spec(Name, Version) ->
	gen_server:call(agner_server, {spec, Name, Version}).

-spec index() -> list(agner_spec_name()).

index() ->
    gen_server:call(agner_server, index).
