%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_github).
-behaviour(agner_index).
-include_lib("agner.hrl").
-include_lib("agner_index.hrl").
-include_lib("kernel/include/file.hrl").

-export([repositories/1,
		 repository/2,
		 tags/2,
		 branches/2,
		 spec/3,
         spec_url/3,
         exists/2
		]).

repositories(Account) ->
    repositories(Account, 1).

repositories(Account, Page) ->
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
                    Repos ++ repositories(Account, Page + 1)
            end
	end.

repository(Account, Name) ->
	case request("https://github.com/api/v2/json/repos/show/" ++ proper_repo_name(Account, Name))  of
		{error, _Reason} = Error ->
			Error;
		Object ->
			proplists:get_value(<<"repository">>, Object)
	end.
	
	
tags(Account, Name) ->
    Port = agner_download:git(["ls-remote", "-t",  "git://github.com/" ++ proper_repo_name(Account, Name) ++ ".git"],
                              [use_stdio, stderr_to_stdout, {line, 255}]),
    PortHandler = fun (F,Acc) ->
                          receive
                              {'EXIT', Port, _} ->
                                  error;
                              {Port,{exit_status,0}} ->
                                  {ok, Acc};
                              {Port,{exit_status,_}} ->
                                  error;
                              {Port, {data, {_, Tag0}}} when is_list(Tag0) ->
                                  [SHA1, "refs/tags/" ++ Tag] = string:tokens(Tag0,"\t"),
                                  F(F,[{Tag, SHA1}|Acc]);
                              _ ->
                                  F(F,Acc)
                          end
                  end,
    Result = PortHandler(PortHandler,[]),
    case Result of
        error ->
            [];
        {ok, Tags} ->
            Tags
    end.

branches(Account, Name) ->
    Port = agner_download:git(["ls-remote", "-h",  "git://github.com/" ++ proper_repo_name(Account, Name) ++ ".git"],
                              [use_stdio, stderr_to_stdout, {line, 255}]),
    PortHandler = fun (F,Acc) ->
                          receive
                              {'EXIT', Port, _} ->
                                  error;
                              {Port,{exit_status,0}} ->
                                  {ok, Acc};
                              {Port,{exit_status,_}} ->
                                  error;
                              {Port, {data, {_, Branch0}}} when is_list(Branch0) ->
                                  [SHA1, "refs/heads/" ++ Branch] = string:tokens(Branch0,"\t"),
                                  F(F,[{Branch, SHA1}|Acc]);
                              _ ->
                                  F(F,Acc)
                          end
                  end,
    Result = PortHandler(PortHandler,[]),
    case Result of
        error ->
            [];
        {ok, Branches} ->
            Branches
    end.

spec(Account, Name, Version) ->
    {ok, RepoServer} = agner_repo_server:create(Name, Version),
    case agner_repo_server:pushed_at(RepoServer) of
        At when is_list(At) ->
            DotDir = filename:join("/tmp","agner"),
            filelib:ensure_dir(DotDir ++ "/"),
            {ok, #file_info{mode = Mode}} = file:read_file_info(DotDir),
            file:change_mode(DotDir, Mode bor 8#0222 bor 8#0444),
            AtFilename = filename:join([DotDir, "cache." ++ Name ++ integer_to_list(erlang:phash2(Version)) ++ 
                                            lists:map(fun ($/) ->
                                                              $_;
                                                          ($\s) ->
                                                              $_;
                                                          (C) ->
                                                              C
                                                      end, At)]),
            spec_1(Account, RepoServer, AtFilename);
        undefined ->
            {A,B,C} = now(),
            N = node(),
            TmpFile = lists:flatten(io_lib:format("/tmp/agner-~p-~p.~p.~p",[N,A,B,C])),
            Result = spec_1(Account, RepoServer, TmpFile),
            file:delete(TmpFile),
            Result
    end.

spec_1(Account, RepoServer,  AtFilename) ->
    Spec = 
        case file:read_file_info(AtFilename) of
            {error, enoent} ->
                case agner_repo_server:clone(RepoServer, fun (Name) -> "git://github.com/" ++ proper_repo_name(Account, Name) ++ ".git" end) of
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
        end,
    case Spec of
        {error, _} = Error ->
            Error;
        _ ->
            agner_spec:normalize(Spec)
    end.

spec_url(Account, Name, SHA1) ->
    "https://github.com/" ++ proper_repo_name(Account, Name) ++ "/blob/" ++ SHA1 ++ "/agner.config".

exists(Account, Name) ->
    Port = agner_download:git(["ls-remote", "-h",  "git://github.com/" ++ proper_repo_name(Account, Name) ++ ".git"],
                              [use_stdio, stderr_to_stdout, {line, 255}]),
    PortHandler = fun (F) ->
                          receive
                              {'EXIT', Port, _} ->
                                  false;
                              {Port,{exit_status,0}} ->
                                  true;
                              {Port,{exit_status,_}} ->
                                  false;
                              {Port, {data, _}} ->
                                  F(F);
                              _ ->
                                  F(F)
                          end
                  end,
    PortHandler(PortHandler).

%%%

proper_repo_name(Account, Name) ->
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
				  [{body_format, binary}|Opts],
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
		 
