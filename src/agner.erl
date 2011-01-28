%% -*- Mode: Erlang; tab-width: 4 -*-
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

arg_proplist() ->
	[{"spec",
	  {spec,
	   "Output the specification of a package",
	   [
		{package, undefined, undefined, string, "Package name"},
		{browser, $b, "browser", boolean, "Show specification in the browser"},
		{homepage, $h, "homepage", boolean, "Show package homepage in the browser"},
		{version, $v, "version", {string, "@master"}, "Version"}
	   ]}},
	 {"versions",
	  {versions,
	  "Show the avilable releases and flavours of a package",
	   [
		{package, undefined, undefined, string, "Package name"}
	   ]}},
	 {"list",
	  {list,
	   "List packages on stdout",
	   [
		{descriptions, $d, "descriptions", {boolean, false}, "Show package descriptions"}
	   ]}},
	 {"fetch",
	  {fetch,
	   "Download a package",
	   [
		{package, undefined, undefined, string, "Package name"},
		{directory, undefined, undefined, string, "Directory to check package out to"},
		{version, $v, "version", {string, "@master"}, "Version"}
	   ]}},
	 {"verify",
	  {verify,
	   "Verify the integrity of a .agner configuration file",
	   [
		{spec, undefined, undefined, {string, "agner.config"}, "Specification file (agner.config by default)"}
	   ]}}].

command_descriptions() ->
	[{Cmd, Desc} || {Cmd, {_Atom, Desc, _Opts}} <- arg_proplist()].

parse_args([Arg|Args]) ->
	case proplists:get_value(Arg, arg_proplist()) of
		undefined -> no_parse;
		{A, _Desc, OptSpec} -> {arg, A, Args, OptSpec}
	end;
parse_args(_) -> no_parse.

usage() ->
    OptSpec = [
               {command, undefined, undefined, string, "Command to be executed (e.g. spec)"}
               ],
    getopt:usage(OptSpec, "agner", "[options ...]"),
	io:format("Valid commands are:~n", []),
	[io:format("   ~-10s ~s~n", [Cmd, Desc]) || {Cmd, Desc} <- command_descriptions()].

main(Args) ->
	case parse_args(Args) of
		{arg, Command, ExtraArgs, OptSpec} ->
			start(),
			{ok, {Opts, _}} = getopt:parse(OptSpec, ExtraArgs),
			handle_command(Command, Opts),
			stop();
		no_parse ->
			usage()
	end.

handle_command(spec, Opts) ->
    case proplists:get_value(package, Opts) of
        undefined ->
            io:format("ERROR: Package name required.~n");
        Package ->
            Version = proplists:get_value(version, Opts),
            case proplists:get_value(browser, Opts) of
                true ->
                    agner_utils:launch_browser(spec_url(Package, Version));
                _ ->
                    ignore
            end,
            Spec = spec(Package,Version),
            case proplists:get_value(homepage, Opts) of
                true ->
                    agner_utils:launch_browser(proplists:get_value(homepage, Spec, "http://google.com/?q=" ++ Package));
                _ ->
                    ignore
            end,
            io:format("~p~n",[Spec])
    end;

handle_command(versions, Opts) ->
    case proplists:get_value(package, Opts) of
        undefined ->
            io:format("ERROR: Package name required.~n");
        Package ->
            io:format("~s",[plists:map(fun (Version) ->
                                               io_lib:format("~s~n",[agner_spec:version_to_list(Version)])
                                       end,
                                       versions(Package))])
    end;

handle_command(list, Opts) ->
    ShowDescriptions = proplists:get_value(descriptions, Opts),
    io:format("~s",[lists:usort(plists:map(fun (Name) ->
                                                   case ShowDescriptions of
                                                       true ->
                                                           io_lib:format("~-40s ~s~n",[Name, proplists:get_value(description, spec(Name))]);
                                                       false ->
                                                           io_lib:format("~s~n",[Name])
                                                   end
                                           end,index()))
                     ]);

handle_command(fetch, Opts) ->
    case proplists:get_value(package, Opts) of
        undefined ->
            io:format("ERROR: Package name required.~n");
        Package ->
            Version = proplists:get_value(version, Opts),
            io:format("~p~n",[fetch(Package,Version,
                                    proplists:get_value(directory, Opts, Package))]),
            case proplists:get_value(caveats, spec(Package, Version)) of
                undefined ->
                    ignore;
                Caveats when is_list(Caveats) ->
                    io:format("=== CAVEATS ===~n~n~s~n~n",[Caveats])
            end
    end;

handle_command(verify, Opts) ->
    SpecFile = proplists:get_value(spec, Opts),
    case file:consult(SpecFile) of
        {error, Reason} ->
            io:format("ERROR: Can't read ~s: ~p~n",[SpecFile, Reason]);
        {ok, Spec} ->
            URL = proplists:get_value(url, Spec),
			TmpFile = temp_name(),
            case (catch agner_download:fetch(URL,TmpFile)) of
                ok ->
                    io:format("~nPASSED~n");
                {'EXIT', {Reason, _}} ->
                    io:format("~nEROR: Can't fetch ~p: ~p~n",[URL, Reason]);
                {error, Reason} ->
                    io:format("~nEROR: Can't fetch ~p: ~p~n",[URL, Reason])
            end,
            os:cmd("rm -rf " ++ TmpFile)
    end.

temp_name() ->
	%% Yes, the temp_name function lives in the test_server, go figure!
	test_server:temp_name("/tmp/agner").

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
