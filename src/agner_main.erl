%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_main).
-export([main/1]).

start() ->
    agner:start().

stop() ->
    error_logger:delete_report_handler(error_logger_tty_h),
    agner:stop().
 
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
                    agner_utils:launch_browser(agner:spec_url(Package, Version));
                _ ->
                    ignore
            end,
            Spec = agner:spec(Package,Version),
            case proplists:get_value(homepage, Opts) of
                true ->
                    agner_utils:launch_browser(proplists:get_value(homepage, Spec, "http://google.com/?q=" ++ Package));
                _ ->
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
                                       agner:versions(Package))])
    end,
	stop();

main(["list"|Args]) ->
    OptSpec = [
               {descriptions, $d, "descriptions", {boolean, false}, "Show package descriptions"},
               {properties, $p, "properties", string, "Comma-separated list of properties to show"}
              ],
	start(),
    {ok, {Opts, _}} = getopt:parse(OptSpec, Args),
    ShowDescriptions = proplists:get_value(descriptions, Opts),
    Properties = lists:map(fun list_to_atom/1, string:tokens(proplists:get_value(properties, Opts,""),",")),
    io:format("~s",[lists:usort(plists:map(fun (Name) ->
                                                   Spec = agner:spec(Name),
                                                   Result0 = case ShowDescriptions of 
                                                       true ->
                                                           io_lib:format("~-40s ~s",[Name, proplists:get_value(description, Spec)]);
                                                       false ->
                                                           io_lib:format("~s",[Name])
                                                   end,
                                                   Result = case Properties of
                                                                [] ->
                                                                    Result0;
                                                                [_|_] ->
                                                                    [Result0|lists:map(fun (Prop) ->
                                                                                               case lists:keyfind(Prop, 1, Spec) of
                                                                                                   false ->
                                                                                                       [];
                                                                                                   T ->
                                                                                                       Val = list_to_tuple(tl(tuple_to_list(T))),
                                                                                                       
                                                                                                       io_lib:format(" | ~s: ~p",[Prop,
                                                                                                                          Val])
                                                                                               end
                                                                                    end, Properties)]
                                                   end,
                                                   Result ++ [$\n]
                                           end,agner:index()))
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
            Version = proplists:get_value(version, Opts),
            io:format("~p~n",[agner:fetch(Package,Version,
                                    proplists:get_value(directory, Opts, Package))]),
            case proplists:get_value(caveats, agner:spec(Package, Version)) of
                undefined ->
                    ignore;
                Caveats when is_list(Caveats) ->
                    io:format("=== CAVEATS ===~n~n~s~n~n",[Caveats])
            end
    end,
	stop();

main(["verify"|Args]) ->
    OptSpec = [
               {spec, undefined, undefined, {string, "agner.config"}, "Specification file (agner.config by default)"}
              ],
	start(),
    {ok, {Opts, _}} = getopt:parse(OptSpec, Args),
    SpecFile = proplists:get_value(spec, Opts),
    case file:consult(SpecFile) of
        {error, Reason} ->
            io:format("ERROR: Can't read ~s: ~p~n",[SpecFile, Reason]);
        {ok, Spec} ->
            URL = proplists:get_value(url, Spec),
            {A,B,C} = now(),
            N = node(),
            TmpFile = lists:flatten(io_lib:format("/tmp/agner-~p-~p.~p.~p",[N,A,B,C])),
            case (catch agner_download:fetch(URL,TmpFile)) of
                ok ->
                    io:format("~nPASSED~n");
                {'EXIT', {Reason, _}} ->
                    io:format("~nEROR: Can't fetch ~p: ~p~n",[URL, Reason]);
                {error, Reason} ->
                    io:format("~nEROR: Can't fetch ~p: ~p~n",[URL, Reason])
            end,
            os:cmd("rm -rf " ++ TmpFile)
    end,
	stop();
    

main(_) ->
    OptSpec = [
               {command, undefined, undefined, string, "Command to be executed (e.g. spec)"}
               ],
    getopt:usage(OptSpec, "agner", "[options ...]").

