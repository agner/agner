%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_github, [Account]).
-behaviour(agner_index).
-include_lib("agner.hrl").
-include_lib("agner_index.hrl").

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
		{struct, Object} ->
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
                                         lists:map(fun ({struct, RepObject}) ->
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
		{struct, Object} ->
			{struct, Repo} = proplists:get_value(<<"repository">>, Object),
			Repo
	end.
	
	
tags(Name) ->
	case request("https://github.com/api/v2/json/repos/show/" ++ proper_repo_name(Name) ++  "/tags") of
		{struct, Object} ->
			{struct, Tags} = proplists:get_value(<<"tags">>, Object),
			lists:map(fun({Tag, SHA1}) ->
							  {binary_to_list(Tag),
							   binary_to_list(SHA1)}
					  end, Tags)
	end.


branches(Name) ->
	case request("https://github.com/api/v2/json/repos/show/" ++ proper_repo_name(Name) ++  "/branches") of
		{error, _Reason} = Error ->
			Error;
		{struct, Object} ->
			{struct, Branches} = proplists:get_value(<<"branches">>, Object),
			lists:map(fun({Branch, SHA1}) ->
							  {binary_to_list(Branch),
							   binary_to_list(SHA1)}
					  end, Branches)
	end.

spec(Name, Version) ->
    {ok, RepoServer} = agner_repo_server:create(Name, Version),
    case agner_repo_server:pushed_at(RepoServer) of
        At when is_list(At) ->
            DotDir = filename:join("/tmp",".agner"),
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
            ok = agner_repo_server:clone(RepoServer, fun (Name) -> "git://github.com/" ++ proper_repo_name(Name) ++ ".git" end),
            
            Config = agner_repo_server:file(RepoServer, "agner.config"),
            {ok, S} = file:consult(Config),
            {ok, _} = file:copy(Config, AtFilename),
            S;
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
				  Opts,
				  agner).

parse_response({ok, {{"HTTP/1.1",200,_},_Headers,Body}}) ->
	mochijson2:decode(Body);
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
		 
