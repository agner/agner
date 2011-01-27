-module(agner).
-include_lib("agner.hrl").
-export([start/0,stop/0]).
-export([main/1]).
%% API
-export([spec/1, spec/2, spec_url/1, spec_url/2, index/0, fetch/2, fetch/3, versions/1]).

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
               {package, undefined, undefined, string, "Package name"},
               {browser, $b, "browser", boolean, "Show specification in the browser"},
               {homepage, $h, "homepage", boolean, "Show package homepage in the browser"},
               {version, $v, "version", {string, "@master"}, "Version"}
              ],
	start(),
    {ok, {Opts, _}} = getopt:parse(OptSpec, Args),
    case proplists:get_value(package, Opts) of
        undefined ->
            io:format("ERROR: Package name required.~n");
        Package ->
            Version = proplists:get_value(version, Opts),
            case proplists:get_value(browser, Opts) of
                true ->
                    agner_utils:launch_browser(spec_url(Package, Version));
                false ->
                    ignore
            end,
            Spec = spec(Package,Version),
            case proplists:get_value(homepage, Opts) of
                true ->
                    agner_utils:launch_browser(proplists:get_value(homepage, Spec, "http://google.com/?q=" ++ Package));
                false ->
                    ignore
            end,
            io:format("~p~n",[Spec])
    end,
	stop();

main(["versions"|Args]) ->
    OptSpec = [
               {package, undefined, undefined, string, "Package name"}
              ],
	start(),
    {ok, {Opts, _}} = getopt:parse(OptSpec, Args),
    case proplists:get_value(package, Opts) of
        undefined ->
            io:format("ERROR: Package name required.~n");
        Package ->
            io:format("~s",[plists:map(fun (Version) ->
                                               io_lib:format("~s~n",[agner_spec:version_to_list(Version)])
                                       end,
                                       versions(Package))])
    end,
	stop();

main(["list"|Args]) ->
    OptSpec = [
               {descriptions, $d, "descriptions", {boolean, false}, "Show package descriptions"}
              ],
	start(),
    {ok, {Opts, _}} = getopt:parse(OptSpec, Args),
    ShowDescriptions = proplists:get_value(descriptions, Opts),
    io:format("~s",[plists:map(fun (Name) ->
                                       case ShowDescriptions of
                                           true ->
                                               io_lib:format("~-40s ~s~n",[Name, proplists:get_value(description, spec(Name))]);
                                           false ->
                                               io_lib:format("~s~n",[Name])
                                       end
                               end,index())
                     ]),
	stop();

main(["fetch"|Args]) ->
    OptSpec = [
               {package, undefined, undefined, string, "Package name"},
               {directory, undefined, undefined, string, "Directory to check package out to"},
               {version, $v, "version", {string, "@master"}, "Version"}
              ],
	start(),
    {ok, {Opts, _}} = getopt:parse(OptSpec, Args),
    case proplists:get_value(package, Opts) of
        undefined ->
            io:format("ERROR: Package name required.~n");
        Package ->
            io:format("~p~n",[fetch(Package,proplists:get_value(version, Opts),
                                    proplists:get_value(directory, Opts, Package))])
    end,
	stop();

main(_) ->
    OptSpec = [
               {command, undefined, undefined, string, "Command to be executed (e.g. spec)"}
               ],
    getopt:usage(OptSpec, "agner", "[options ...]").


%%%===================================================================
%%% API
%%%===================================================================
-spec spec(agner_spec_name()) -> agner_spec() | not_found_error().
-spec spec(agner_spec_name(), agner_spec_version() | string()) -> agner_spec() | not_found_error().

spec(Name) ->
	spec(Name, {branch, "master"}).

spec(Name, Version) when is_atom(Name) ->
	spec(atom_to_list(Name),Version);

spec(Name, Version) when is_list(Version) ->
    spec(Name, agner_spec:list_to_version(Name, Version));

spec(Name, Version) ->
	gen_server:call(agner_server, {spec, Name, Version}).

-spec spec_url(agner_spec_name()) -> url() | not_found_error().
-spec spec_url(agner_spec_name(), agner_spec_version() | string()) -> url() | not_found_error().

spec_url(Name) ->
	spec_url(Name, {branch, "master"}).

spec_url(Name, Version) when is_atom(Name) ->
	spec_url(atom_to_list(Name),Version);

spec_url(Name, Version) when is_list(Version) ->
    spec_url(Name, agner_spec:list_to_version(Name, Version));

spec_url(Name, Version) ->
	gen_server:call(agner_server, {spec_url, Name, Version}).




-spec index() -> list(agner_spec_name()).

index() ->
    gen_server:call(agner_server, index).

-spec fetch(agner_spec_name(), directory()) -> ok | not_found_error().
-spec fetch(agner_spec_name(), agner_spec_version(), directory()) -> ok | not_found_error().

fetch(Name, Directory) ->
    fetch(Name, {branch, "master"}, Directory).

fetch(Name, Version, Directory) when is_list(Version) ->
    fetch(Name, agner_spec:list_to_version(Name, Version), Directory);
fetch(Name, Version, Directory) ->
    gen_server:call(agner_server, {fetch, Name, Version, Directory}, infinity). %% fetch might be time consuming

-spec versions(agner_spec_name()) -> list(agner_spec_version()).

versions(Name) ->
    gen_server:call(agner_server, {versions, Name}).
                      
                   
