%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_main).
-export([main/1]).

start() ->
    agner:start().

stop() ->
    error_logger:delete_report_handler(error_logger_tty_h),
    agner:stop().

arg_proplist() ->
	[{"version",
      {version,
       "Agner version",
       []}},
     {"help",
      {help,
       "Use agner help <command>",
       [{command, undefined, undefined, string, "Command"}]}},
     {"spec",
	  {spec,
	   "Output the specification of a package",
	   [
		{package, undefined, undefined, string, "Package name"},
		{browser, $b, "browser", boolean, "Show specification in the browser"},
		{homepage, $h, "homepage", boolean, "Show package homepage in the browser"},
		{version, $v, "version", {string, "@master"}, "Version"},
        {property, $p, "property", string, "Particular property to render instead of a full spec"},
        {spec, $s, "spec-file", string, "Use local specification file"}
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
		{descriptions, $d, "descriptions", {boolean, false}, "Show package descriptions"},
		{properties, $p, "properties", string, "Comma-separated list of properties to show"},
        {search, $s, "search", string, "Keyword to search"}
	   ]}},
     {"search",
      {search,
       "Search packages",
       [
        {search, undefined, undefined, string, "Keyword to search"},
		{descriptions, $d, "descriptions", {boolean, false}, "Show package descriptions"},
		{properties, $p, "properties", string, "Comma-separated list of properties to show"}
       ]}},
	 {"fetch",
	  {fetch,
	   "Download a package",
	   [
		{package, undefined, undefined, string, "Package name"},
		{directory, undefined, undefined, string, "Directory to check package out to"},
		{version, $v, "version", {string, "@master"}, "Version"},
        {build, $b, "build", {boolean, false}, "Build fetched package"},
        {addpath, $a, "add-path", {boolean, false}, "Add path to compiled package to .erlang"},
        {install, $i, "install", {boolean, false}, "Install package (if install_command is available)"},
        {spec, $s, "spec-file", string, "Use local specification file"}
	   ]}},
     {"install",
      {install,
       "Install a package",
	   [
		{package, undefined, undefined, string, "Package name"},
		{version, $v, "version", {string, "@master"}, "Version"},
        {spec, $s, "spec-file", string, "Use local specification file"}
	   ]}},
     {"uninstall",
      {uninstall,
       "Uninstall previously installed package",
	   [
		{package, undefined, undefined, string, "Package name"},
		{version, $v, "version", {string, "@master"}, "Version"},
        {spec, $s, "spec-file", string, "Use local specification file"}
	   ]}},
     {"prefix",
      {prefix,
       "Shows location where particular package is installed",
	   [
		{package, undefined, undefined, string, "Package name"},
		{version, $v, "version", {string, "@master"}, "Version"}
	   ]}},
     {"build",
      {build,
       "Build a package",
	   [
		{package, undefined, undefined, string, "Package name"},
		{version, $v, "version", {string, "@master"}, "Version"},
        {spec, $s, "spec-file", string, "Use local specification file"},
        {package_path, undefined, "package-path", string, "Path to the package repo contents (used in conjunction with --spec-file only, defaults to '.')"},
        {addpath, $a, "add-path", {boolean, false}, "Add path to compiled package to .erlang"},
        {install, $i, "install", {boolean, false}, "Install package (if install_command is available)"},
		{directory, undefined, undefined, string, "Directory to check package out to"}
	   ]}},      
	 {"verify",
	  {verify,
	   "Verify the integrity of a .agner configuration file",
	   [
		{spec, undefined, undefined, {string, "agner.config"}, "Specification file (agner.config by default)"}
	   ]}},
     {"config",
      {config,
       "Show Agner's environmental configuration",
       [
        {variable, undefined, undefined, string, "Variable name, omit to list all of them"}
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
    os:putenv("AGNER", filename:absname(escript:script_name())),
	case parse_args(Args) of
		{arg, Command, ExtraArgs, OptSpec} ->
			case getopt:parse(OptSpec, ExtraArgs) of
				{ok, {Opts, _}} ->
					start(),
					handle_command(Command, Opts),
					stop();
			    {error, {missing_option_arg, Arg}} ->
					io:format("Error: Missing option argument for '~p'~n", [Arg])
			end;
		no_parse ->
			usage()
	end.

handle_command(help, []) ->
    usage();
handle_command(help, Opts) ->
    case proplists:get_value(command, Opts) of
        undefined ->
            usage();
        Command ->
            case proplists:get_value(Command, arg_proplist()) of
                {_Atom, _Desc, Opts1} ->
                    getopt:usage(Opts1, "agner " ++ Command);
                undefined ->
                    io:format("No such command: ~s~n", [Command])
            end
    end;

handle_command(spec, Opts) ->
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
            Spec = 
                case proplists:get_value(spec, Opts) of
                    undefined ->
                        agner:spec(Package, Version);
                    File ->
                        {ok, T} = file:consult(File),
                        T
                end,
            case proplists:get_value(homepage, Opts) of
                true ->
                    agner_utils:launch_browser(proplists:get_value(homepage, Spec, "http://google.com/#q=" ++ Package));
                _ ->
                    ignore
            end,
            case proplists:get_value(property, Opts) of
                undefined ->
                    io:format("~p~n",[Spec]);
                Property ->
                    io:format("~s~n",[agner_spec:property_to_list(lists:keyfind(list_to_atom(Property), 1, Spec))])
            end
    end;

handle_command(versions, Opts) ->
    case proplists:get_value(package, Opts) of
        undefined ->
            io:format("ERROR: Package name required.~n");
        Package ->
            io:format("~s",[plists:map(fun (Version) ->
                                               io_lib:format("~s~n",[agner_spec:version_to_list(Version)])
                                       end,
                                       agner:versions(Package))])
    end;

handle_command(search, Opts) ->
    handle_command(list, Opts);

handle_command(list, Opts) ->
    ShowDescriptions = proplists:get_value(descriptions, Opts),
    Search = proplists:get_value(search, Opts),
    Properties = lists:map(fun list_to_atom/1, string:tokens(proplists:get_value(properties, Opts,""),",")),
    io:format("~s",[lists:usort(plists:map(fun (Name) ->
                                                   Spec = agner:spec(Name),
                                                   Searchable = string:to_lower(lists:flatten([Name,proplists:get_value(description,Spec,[])|proplists:get_value(keywords,Spec,[])])),
                                                   Show = 
                                                       case Search of
                                                           undefined ->
                                                                true;
                                                           [_|_] ->
                                                               string:rstr(Searchable, string:to_lower(Search)) > 0
                                                       end,
                                                   case Show of
                                                       true ->
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
                                                           Result ++ [$\n];
                                                        false ->
                                                           ""
                                                   end
                                           end,agner:index()))
                   ]);

handle_command(prefix, Opts) ->
    case proplists:get_value(package, Opts) of
        undefined ->
            io:format("ERROR: Package name required.~n");
        Package ->
            Version = proplists:get_value(version, Opts),
            InstallPrefix = filename:join([os:getenv("AGNER_PREFIX"),"packages",Package ++ "-" ++ Version]),
            case filelib:is_dir(InstallPrefix) of
                true ->
                    io:format("~s~n",[InstallPrefix]);
                false ->
                    ignore
            end
    end;

handle_command(uninstall, Opts) ->
    case proplists:get_value(package, Opts) of
        undefined ->
            io:format("ERROR: Package name required.~n");
        Package ->
            Version = proplists:get_value(version, Opts),
            InstallPrefix = filename:join([os:getenv("AGNER_PREFIX"),"packages",Package ++ "-" ++ Version]),
            case filelib:is_dir(InstallPrefix) of
                true ->
                    io:format("Uninstalling...~n"),
                    Spec = 
                        case proplists:get_value(spec, Opts) of
                            undefined ->
                                agner:spec(Package, Version);
                            File ->
                        {ok, T} = file:consult(File),
                                T
                        end,
                    os:cmd("rm -rf " ++ InstallPrefix),
                    case proplists:get_value(bin_files, Spec) of
                        undefined ->
                            ignore;
                        Files ->
                            lists:foreach(fun (File) ->
                                                  Symlink = filename:join(os:getenv("AGNER_BIN"),filename:basename(File)),
                                                  file:delete(Symlink)
                                          end, Files)
                    end;
                false ->
                    io:format("ERROR: This package hasn't been installed~n")
            end
    end;

handle_command(install, Opts) ->
    TmpFile = temp_name(),
    handle_command(fetch, [{build, true},{directory, TmpFile},{install, true},{addpath, false}|Opts]),
    os:cmd("rm -rf " ++ TmpFile);

handle_command(build, Opts) ->
    handle_command(fetch, [{build, true}|Opts]);

handle_command(fetch, Opts) ->
    case proplists:get_value(package, Opts) of
        undefined ->
            io:format("ERROR: Package name required.~n");
        Package ->
            Version = proplists:get_value(version, Opts),
            Directory = filename:absname(proplists:get_value(directory, Opts, Package)),
            Spec = 
                case proplists:get_value(spec, Opts) of
                    undefined ->
                        Spec0 = agner:spec(Package, Version),
                        {ok, RepoServer} = agner_repo_server:create(Package, agner_spec:list_to_version(Package, Version)),
                        
                        os:putenv("AGNER_PACKAGE_REPO",agner_repo_server:file(RepoServer,"")),
                        Spec0;
                    File ->
                        {ok, T} = file:consult(File),
                        os:putenv("AGNER_PACKAGE_REPO",proplists:get_value(package_path, Opts, ".")),
                        T
                end,

            io:format("~p~n",[agner:fetch(Spec,Version,
                                    Directory)]),

                          
            case proplists:get_value(caveats, Spec) of
                undefined ->
                    ignore;
                Caveats when is_list(Caveats) ->
                    io:format("=== CAVEATS ===~n~n~s~n~n",[Caveats])
            end,

            case proplists:get_value(build, Opts) of
                true ->
                    case proplists:get_value(rebar_compatible, Spec) of
                        true ->
                            io:format("Building...~n"),
                            {ok, Cwd} = file:get_cwd(),
                            file:set_cwd(Directory),
                            rebar:main(["get-deps"]),
                            rebar:main(["compile"]),
                            file:set_cwd(Cwd);
                        _ ->
                            case proplists:get_value(build_command, Spec) of
                                undefined ->
                                    io:format("ERROR: No build_command specified, can't build this package");
                                Command ->
                                    io:format("Building (output will be shown when done)...~n"),
                                    {ok, Cwd} = file:get_cwd(),
                                    file:set_cwd(Directory),
                                    io:format("~s~n",[os:cmd(Command)]),
                                    file:set_cwd(Cwd)
                            end
                    end,
                    case proplists:get_value(addpath, Opts) of
                        true ->
                            {ok, F} = file:open(filename:join(os:getenv("HOME"),".erlang"),
                                                [append]),
                            file:write(F, io_lib:format("code:add_patha(\"~s/ebin\"). %% {agner, ~s}~n", [Directory, Package])),
                            file:close(F);
                        false ->
                            ignore
                    end,
                    case proplists:get_value(install, Opts) of
                        false ->
                            ignore;
                        true ->
                            filelib:ensure_dir(filename:join([os:getenv("AGNER_PREFIX"),"packages"]) ++ "/"),
                            InstallPrefix = filename:join([os:getenv("AGNER_PREFIX"),"packages",Package ++ "-" ++ Version]),
                            os:cmd("rm -rf " ++ InstallPrefix),
                            ok = filelib:ensure_dir(InstallPrefix ++ "/"),
                            os:putenv("AGNER_INSTALL_PREFIX", InstallPrefix),
                            case proplists:get_value(install_command, Spec) of
                                undefined ->
                                    io:format("ERROR: No install_command specified, can't install this package");
                                ICommand ->
                                    io:format("Installing (output will be shown when done)...~n"),
                                    {ok, Cwd1} = file:get_cwd(),
                                    file:set_cwd(Directory),
                                    io:format("~s~n",[os:cmd(ICommand)]),
                                    file:set_cwd(Cwd1),
                                    case proplists:get_value(bin_files, Spec) of
                                        undefined ->
                                            ignore;
                                        Files ->
                                            lists:foreach(fun (File) ->
                                                                  Symlink = filename:join(os:getenv("AGNER_BIN"),filename:basename(File)),
                                                                  File1 = filename:join([InstallPrefix,File]),
                                                                  file:delete(Symlink),
                                                                  ok = file:make_symlink(File1, Symlink)
                                                          end, Files)
                                    end
                            end
                    end;
                false ->
                    ignore
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
    end;

handle_command(version, _) ->
    {agner,_,Version} = lists:keyfind(agner,1,application:which_applications()),
    io:format("~s~n",[Version]);

handle_command(config, []) ->
    io:format("prefix="), handle_command(config,[{variable, "prefix"}]),
    io:format("bin="), handle_command(config,[{variable, "bin"}]);
handle_command(config, [{variable, "prefix"}]) ->
    io:format("~s~n",[os:getenv("AGNER_PREFIX")]);
handle_command(config, [{variable, "bin"}]) ->
    io:format("~s~n",[os:getenv("AGNER_BIN")]).
  

%%%%

temp_name() ->
	%% Yes, the temp_name function lives in the test_server, go figure!
	test_server:temp_name("/tmp/agner").


