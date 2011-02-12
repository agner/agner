%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_github, [Account]).
-behaviour(agner_index).
-include_lib("agner.hrl").
-include_lib("agner_index.hrl").
-include_lib("kernel/include/file.hrl").

-export([repositories/0,
		 repository/1,
		 tags/1,
		 branches/1,
		 spec/2,
         spec_url/2
		]).

repositories() ->
    repositories(1).

repositories(Page) ->
	case request("https://github.com/api/v2/json/repos/show/" ++ Account ++ "?page=" ++ integer_to_list(Page))  of
		{error, _Reason} = Error ->
			Error;
		Object ->
			case proplists:get_value(<<"repositories">>, Object) of
                [] ->
                    [];
                Repositories ->
                    Repos = lists:filter(fun 
                                             ({_,undefined}) ->
                                                 false;
                                             ({invalid,_}) ->
                                                 false;
                                             (_) ->
                                                 true
                                         end,
                                         lists:map(fun (RepObject) ->
                                                           {repo_name(proplists:get_value(<<"name">>, RepObject)),
                                                            proplists:get_value(<<"pushed_at">>, RepObject)}
                                                   end, Repositories)),
                    Repos ++ repositories(Page + 1)
            end
	end.

repository(Name) ->
	case request("https://github.com/api/v2/json/repos/show/" ++ proper_repo_name(Name))  of
		{error, _Reason} = Error ->
			Error;
		Object ->
			proplists:get_value(<<"repository">>, Object)
	end.
	
	
tags(Name) ->
    {ok, RepoServer} = agner_repo_server:create(Name, {flavour, "master"}),
    ok = agner_repo_server:clone(RepoServer, fun (N) -> "git://github.com/" ++ proper_repo_name(N) ++ ".git" end),
    Path = agner_repo_server:file(RepoServer, []),
    Port = agner_download:git(["tag", "-l"], [{cd, Path}, use_stdio, stderr_to_stdout, {line, 255}]),
    PortHandler = fun (F,Acc) ->
                          receive
                              {'EXIT', Port, _} ->
                                  error;
                              {Port,{exit_status,0}} ->
                                  {ok, Acc};
                              {Port,{exit_status,_}} ->
                                  error;
                              {Port, {data, {_, D}}} when is_list(D) ->
                                  Tag = string:strip(D, right, $\n),
                                  F(F,[Tag|Acc]);
                              _ ->
                                  F(F,Acc)
                          end
                  end,
    Result = PortHandler(PortHandler,[]),
    case Result of
        error ->
            [];
        {ok, Tags} ->
            lists:map(fun (Tag) ->
                              RevParsePort = agner_download:git(["rev-parse",Tag], 
                                                                [{cd, Path}, use_stdio, stderr_to_stdout, {line, 255}]),
                              RevParsePortHandler = fun (F1,Acc1) ->
                                                            receive 
                                                                {'EXIT', RevParsePort, _} ->
                                                                    "";
                                                                {RevParsePort,{exit_status,0}} ->
                                                                    Acc1;
                                                                {RevParsePort, {exit_status, _}} ->
                                                                    "";
                                                                {RevParsePort, {data, {_, D1}}} when is_list (D1) ->
                                                                    F1(F1,D1);
                                                                _ ->
                                                                    F1(F1,Acc1)
                                                            end
                                                    end,
                              SHA1 = RevParsePortHandler(RevParsePortHandler,""),
                              {Tag, SHA1}
                      end, Tags)
    end.


branches(Name) ->
    {ok, RepoServer} = agner_repo_server:create(Name, {flavour, "master"}),
    ok = agner_repo_server:clone(RepoServer, fun (N) -> "git://github.com/" ++ proper_repo_name(N) ++ ".git" end),
    Path = agner_repo_server:file(RepoServer, []),
    Port = agner_download:git(["branch", "-r"], [{cd, Path}, use_stdio, stderr_to_stdout, {line, 255}]),
    PortHandler = fun (F,Acc) ->
                          receive
                              {'EXIT', Port, _} ->
                                  error;
                              {Port,{exit_status,0}} ->
                                  {ok, Acc};
                              {Port,{exit_status,_}} ->
                                  error;
                              {Port, {data, {_, "  origin/HEAD" ++ _}}} -> %% ignore
                                  F(F, Acc);
                              {Port, {data, {_, "  origin/" ++ Branch}}} when is_list(Branch) ->
                                  F(F,[Branch|Acc]);
                              {Port, {data, {_, _}}} -> %% ignore as well
                                  F(F,Acc);
                              _ ->
                                  F(F,Acc)
                          end
                  end,
    Result = PortHandler(PortHandler,[]),
    case Result of
        error ->
            [];
        {ok, Branches} ->
            lists:map(fun (Branch) ->
                              RevParsePort = agner_download:git(["rev-parse",Branch], 
                                                                [{cd, Path}, use_stdio, stderr_to_stdout, {line, 255}]),
                              RevParsePortHandler = fun (F1,Acc1) ->
                                                            receive 
                                                                {'EXIT', RevParsePort, _} ->
                                                                    "";
                                                                {RevParsePort,{exit_status,0}} ->
                                                                    Acc1;
                                                                {RevParsePort, {exit_status, _}} ->
                                                                    "";
                                                                {RevParsePort, {data, {_, D1}}} when is_list (D1) ->
                                                                    F1(F1,D1);
                                                                _ ->
                                                                    F1(F1,Acc1)
                                                            end
                                                    end,
                              SHA1 = RevParsePortHandler(RevParsePortHandler,""),
                              {Branch, SHA1}
                      end, Branches)
    end.

spec(Name, Version) ->
    {ok, RepoServer} = agner_repo_server:create(Name, Version),
    case agner_repo_server:pushed_at(RepoServer) of
        At when is_list(At) ->
            DotDir = filename:join("/tmp","agner"),
            filelib:ensure_dir(DotDir ++ "/"),
            AtFilename = filename:join([DotDir, "cache." ++ Name ++ integer_to_list(erlang:phash2(Version)) ++ 
                                            lists:map(fun ($/) ->
                                                              $_;
                                                          ($\s) ->
                                                              $_;
                                                          (C) ->
                                                              C
                                                      end, At)]),
            spec_1(RepoServer, AtFilename);
        undefined ->
            {A,B,C} = now(),
            N = node(),
            TmpFile = lists:flatten(io_lib:format("/tmp/agner-~p-~p.~p.~p",[N,A,B,C])),
            Result = spec_1(RepoServer, TmpFile),
            file:delete(TmpFile),
            Result
    end.

spec_1(RepoServer,  AtFilename) ->
    case file:read_file_info(AtFilename) of
        {error, enoent} ->
            case agner_repo_server:clone(RepoServer, fun (Name) -> "git://github.com/" ++ proper_repo_name(Name) ++ ".git" end) of
                ok ->
                    Config = agner_repo_server:file(RepoServer, "agner.config"),
                    {ok, S} = file:consult(Config),
                    {ok, _} = file:copy(Config, AtFilename),
                    {ok, #file_info{mode = Mode}} = file:read_file_info(AtFilename),
                    file:change_mode(AtFilename, Mode bor 8#00444),
                    S;
                _ ->
                    {error, not_found}
            end;
        {ok, _} ->
            {ok, S} = file:consult(AtFilename),
            S
    end.

spec_url(Name, SHA1) ->
    "https://github.com/" ++ proper_repo_name(Name) ++ "/blob/" ++ SHA1 ++ "/agner.config".

%%%

proper_repo_name(Name) ->
    case string:tokens(Name,"/") of
        [_, _]=L ->
            string:join(L,"/") ++ ".agner";
        [Name] ->
            string:join([Account, Name],"/") ++ ".agner"
    end.

%%%

request(URL) ->
	parse_response(httpc_request(URL)).

httpc_request(URL) ->
	httpc_request_1(URL,[]).

httpc_request_1(URL, Opts) ->
	httpc:request(get,{URL,
					   []},
				  [{timeout, 60000}],
				  [{body_format, binary}] ++ Opts,
				  agner).

parse_response({ok, {{"HTTP/1.1",200,_},_Headers,Body}}) ->
	jsx:json_to_term(Body);
parse_response({ok, {{"HTTP/1.1",404,_},_Headers,_Body}}) ->
	{error, not_found};
parse_response({ok, {{"HTTP/1.1",403,_}, _Headers, _Body}}) ->
    {error, github_rate_limit_exceeded}.
%%%

repo_name(B) when is_binary(B) ->
	S = binary_to_list(B),
	case string:tokens(S,".") of
		[Name,"agner"] ->
			Name;
		_ ->
			invalid
	end.
		 
