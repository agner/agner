%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner).
-include_lib("agner.hrl").
-export([start/0,stop/0]).
-export([main/1]).
%% API
-export([spec/1, spec/2, spec_url/1, spec_url/2, index/0, fetch/2, fetch/3, versions/1]).

-define(AGNER_BIN,"/usr/local/bin").
-define(AGNER_PREFIX,"/usr/local/agner").

start() ->
    case os:getenv("AGNER_BIN") of
        false ->
            os:putenv("AGNER_BIN",?AGNER_BIN);
        [_|_] ->
            ignore
    end,
    case os:getenv("AGNER_PREFIX") of
        false ->
            os:putenv("AGNER_PREFIX",?AGNER_PREFIX);
        [_|_] ->
            ignore
    end,
    ok = filelib:ensure_dir(os:getenv("AGNER_PREFIX") ++ "/"),
	inets:start(),
	ssl:start(),
	{ok, _Pid} = inets:start(httpc,[{profile, agner}]),
    application:start(gproc),
    gproc:start_link(),
	application:start(agner).

stop() ->
	application:stop(agner),
	inets:stop(),
	ssl:stop().

main(Args) ->
    agner_main:main(Args).


%%%===================================================================
%%% API
%%%===================================================================
-spec spec(agner_spec_name()) -> agner_spec() | not_found_error().
-spec spec(agner_spec_name(), agner_spec_version() | string()) -> agner_spec() | not_found_error().

spec(Name) ->
	spec(Name, {flavour, "master"}).

spec(Name, Version) when is_atom(Name) ->
	spec(atom_to_list(Name),Version);

spec(Name, Version) when is_list(Version) ->
    spec(Name, agner_spec:list_to_version(Name, Version));

spec(Name, Version) ->
    agner_server:spec(Name, Version).

-spec spec_url(agner_spec_name()) -> url() | not_found_error().
-spec spec_url(agner_spec_name(), agner_spec_version() | string()) -> url() | not_found_error().

spec_url(Name) ->
	spec_url(Name, {flavour, "master"}).

spec_url(Name, Version) when is_atom(Name) ->
	spec_url(atom_to_list(Name),Version);

spec_url(Name, Version) when is_list(Version) ->
    spec_url(Name, agner_spec:list_to_version(Name, Version));

spec_url(Name, Version) ->
    agner_server:spec_url(Name, Version).




-spec index() -> list(agner_spec_name()).

index() ->
    agner_server:index().


-spec fetch(agner_spec_name(), directory()) -> ok | not_found_error().
-spec fetch(agner_spec_name(), agner_spec_version() | agner_spec_version_string(), directory()) -> ok | not_found_error().

fetch(Name, Directory) ->
    fetch(Name, {flavour, "master"}, Directory).

fetch(Name, Version, Directory) when is_list(Version) ->
    fetch(Name, agner_spec:list_to_version(Name, Version), Directory);
fetch(Name, Version, Directory) ->
    agner_server:fetch(Name, Version, Directory).



-spec versions(agner_spec_name()) -> list(agner_spec_version()).

versions(Name) ->
    agner_server:versions(Name).
