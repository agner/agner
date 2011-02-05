%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_main).
-export([main/1]).
-include_lib("kernel/include/file.hrl").

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
	  "Show the available releases and flavours of a package",
	   [
		{package, undefined, undefined, string, "Package name"},
        {no_flavours, undefined, "no-flavours", {boolean, false}, "Don't show flavour versions"},
        {no_releases, undefined, "no-releases", {boolean, false}, "Don't show release versions"}
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
     {"create",
      {create,
       "Create new .agner repository",
       [
        {package, undefined, undefined, string, "Package name"},
        {github_account, undefined, "github-account",{string, "agner"}, "GitHub account to set as origin"}
       ]}},
	 {"verify",
	  {verify,
	   "Verify the integrity of an .agner configuration file",
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
					Result = (catch handle_command(Command, Opts)),
                    case Result of
                        {'EXIT', {{agner_failure, Reason},_}} ->
                            io:format("ERROR: ~s~n",[Reason]);
                        {'EXIT', Error} ->
                            io:format("FAILURE: ~p~n",[Error]);
                        _ ->
                            ignore
                    end,
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
                    lists:foreach(fun(Property) ->
                                          io:format("~p.~n",[Property])
                                  end, Spec);
                Property ->
                    io:format("~s~n",[agner_spec:property_to_list(lists:keyfind(list_to_atom(Property), 1, Spec))])
            end
    end;

handle_command(versions, Opts) ->
    case proplists:get_value(package, Opts) of
        undefined ->
            io:format("ERROR: Package name required.~n");
        Package ->
            NoFlavours = proplists:get_value(no_flavours, Opts),
            NoReleases = proplists:get_value(no_releases, Opts),
            io:format("~s",[lists:usort(plists:map(fun ({flavour, _} = Version) when not NoFlavours ->
                                                           io_lib:format("~s~n",[agner_spec:version_to_list(Version)]);
                                                       ({release, _} = Version) when not NoReleases ->
                                                           io_lib:format("~s~n",[agner_spec:version_to_list(Version)]);
                                                       (_) ->
                                                           ""
                                                   end,
                                                   agner:versions(Package)))])
    end;

handle_command(search, Opts) ->
    handle_command(list, Opts);

handle_command(list, Opts) ->
    ShowDescriptions = proplists:get_value(descriptions, Opts),
    Search = proplists:get_value(search, Opts),
    Properties = lists:map(fun list_to_atom/1, string:tokens(proplists:get_value(properties, Opts,""),",")),
    lists:foreach(fun (Name) ->
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
                                  case ShowDescriptions of
                                      true ->
                                          io:format("~-40s ~s",[Name, proplists:get_value(description, Spec)]);
                                      false ->
                                          io:format("~s",[Name])
                                  end,
                                  case Properties of
                                      [] ->
                                          ignore;
                                      [_|_] ->
                                          lists:foreach(fun (Prop) ->
                                                                case lists:keyfind(Prop, 1, Spec) of
                                                                    false ->
                                                                        ignore;
                                                                    T ->
                                                                        Val = list_to_tuple(tl(tuple_to_list(T))),
                                                                        io:format(" | ~s: ~p",[Prop,
                                                                                               Val])
                                                                end
                                                        end, Properties)
                                  end,
                                  io:format("~n");
                              false ->
                                  ok
                          end
                  end,agner:index());

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
            case agner_spec:version_to_list(agner_spec:list_to_version(Package, proplists:get_value(version, Opts))) of
                undefined ->
                    io:format("ERROR: No version satisfy criteria of ~s~n",[proplists:get_value(version, Opts)]);
                Version ->
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
                                os:putenv("AGNER_PACKAGE_REPO",proplists:get_value(package_path, Opts, filename:absname("."))),
                                T
                        end,
                    
                    io:format("~p~n",[agner:fetch(Spec,Version,
                                                  Directory)]),
                    
                    Requires = lists:sort(fun ({"agner", _},_) ->
                                                  true;
                                              ("agner", _) ->
                                                  true;
                                              (A,B) ->
                                                  A =< B
                                          end, proplists:get_value(requires, Spec, [])),
                    DepsDir = filename:join(Directory, proplists:get_value(deps_dir, Spec, "deps")),
                    lists:foreach(fun ("agner") ->
                                          ignore;
                                      ({"agner", "atleast:" ++ AgnerVersion}) ->
                                          {agner,_,CurrentAgnerVersion} = lists:keyfind(agner,1,application:which_applications()),
                                          case AgnerVersion > CurrentAgnerVersion of
                                              true ->
                                                  error({agner_failure, "Your agner is too old (" ++ CurrentAgnerVersion ++ ", " ++ AgnerVersion ++ " required)"});
                                              false ->
                                                  ignore
                                          end;
                                      ({"agner", AgnerVersion}) ->
                                          {agner,_,CurrentAgnerVersion} = lists:keyfind(agner,1,application:which_applications()),
                                          case AgnerVersion /= CurrentAgnerVersion of
                                              true ->
                                                  error({agner_failure, "Your agner version is mismatched (" ++ CurrentAgnerVersion ++ ", " ++ AgnerVersion ++ " required)"});
                                              false ->
                                                  ignore
                                          end;
                                      ({ReqName, ReqVersion}) ->
                                          io:format("[Building dependency: ~s -v ~s]~n", [ReqName, ReqVersion]),
                                          handle_command(fetch, [{package, ReqName},{version, ReqVersion},
                                                                 {directory, filename:join(DepsDir,ReqName)}|
                                                                 proplists:delete(spec,Opts)]);
                                      (ReqName) when is_list(ReqName) ->
                                          io:format("[Building dependency: ~s]~n", [ReqName]),
                                          handle_command(fetch, [{package, ReqName},{version, "@master"},
                                                                 {directory, filename:join(DepsDir,ReqName)}|
                                                                 proplists:delete(spec, Opts)])
                                  end, Requires),
                    
                    case proplists:get_value(caveats, Spec) of
                        undefined ->
                            ignore;
                        Caveats when is_list(Caveats) ->
                    io:format("=== CAVEATS ===~n~n~s~n~n",[Caveats])
                    end,
                    InstallPrefix = filename:join([os:getenv("AGNER_PREFIX"),"packages",Package ++ "-" ++ Version]),
                    os:putenv("AGNER_INSTALL_PREFIX", InstallPrefix),
                    
                    case proplists:get_value(build, Opts) of
                        true ->
                            case proplists:get_value(rebar_compatible, Spec) of
                                true ->
                                    io:format("[Building...]~n"),
                                    {ok, Cwd} = file:get_cwd(),
                                    file:set_cwd(Directory),
                                    RebarCommands = proplists:get_value(rebar_commands, Spec,["get-deps","compile"]),
                                    rebar_config:set_global(shutdown_agner, false), %% prevents rebar from shutting down agner
                                    rebar:main(RebarCommands),
                                    file:set_cwd(Cwd);
                                _ ->
                                    ignore
                            end,
                            case proplists:get_value(build_command, Spec) of
                                undefined ->
                                    case proplists:get_value(rebar_compatible, Spec) of
                                        true ->
                                            ignore;
                                        _ ->
                                            io:format("ERROR: No build_command specified, can't build this package~n")
                                    end;
                                Command ->
                                    io:format("[Building (output will be shown when done)...]~n"),
                            {ok, Cwd0} = file:get_cwd(),
                                    file:set_cwd(Directory),
                                    io:format("~s~n",[os:cmd(Command)]),
                                    file:set_cwd(Cwd0)
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
                                    os:cmd("rm -rf " ++ InstallPrefix),
                                    ok = filelib:ensure_dir(InstallPrefix ++ "/"),
                                    case proplists:get_value(install_command, Spec) of
                                        undefined ->
                                            io:format("ERROR: No install_command specified, can't install this package~n");
                                        ICommand ->
                                            io:format("[Installing (output will be shown when done)...]~n"),
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
      
                                                                          {ok, #file_info{mode = Mode}} = file:read_file_info(File1),

                                                                          file:change_mode(File1, Mode bor 8#00011),
                                                                          ok = file:make_symlink(File1, Symlink)
                                                                  end, Files)
                                            end
                                    end
                            end;
                        false ->
                            ignore
                    end
            end
    end;

handle_command(create, Opts) ->
    case proplists:get_value(package, Opts) of
        undefined ->
            io:format("ERROR: Package name required.~n");
        Package ->
            Dir = filename:absname(Package ++ ".agner"),
            ClonePort = agner_download:git(["clone","-q","https://github.com/agner/agner.template.git",Dir]),
            agner_download:process_port(ClonePort, fun() ->
                                                           agner_download:git(["config","remote.origin.url","git@github.com:" ++ proplists:get_value(github_account, Opts) ++ "/" ++ Package ++ ".agner.git"],[{cd, Dir}])
                                                   end)
    end;

handle_command(verify, Opts) ->
    SpecFile = proplists:get_value(spec, Opts),
    case file:consult(SpecFile) of
        {error, Reason} ->
            io:format("ERROR: Can't read ~s: ~p~n",[SpecFile, Reason]);
        {ok, Spec} ->
            URL = proplists:get_value(url, Spec),
			TmpFile = temp_name(),
            case (catch agner_download:fetch(Spec,TmpFile)) of
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


