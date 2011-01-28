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
	case request("https://github.com/api/v2/json/repos/show/" ++ Account)  of
		{error, _Reason} = Error ->
			Error;
		{struct, Object} ->
			Repositories = proplists:get_value(<<"repositories">>, Object),
			lists:filter(fun ({invalid,_}) ->
							  false;
						  (_) ->
							  true
					  end,
					  lists:map(fun ({struct, RepObject}) ->
										{repo_name(proplists:get_value(<<"name">>, RepObject)),
                                         proplists:get_value(<<"pushed_at">>, RepObject)}
								end, Repositories))
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

spec(Name, SHA1) ->
    case gen_server:call(agner_server, {pushed_at, Name}) of
        At when is_list(At) ->
            DotDir = filename:join(os:getenv("HOME"),".agner"),
            filelib:ensure_dir(DotDir ++ "/"),
            AtFilename = filename:join([DotDir, "cache." ++ Name ++ SHA1 ++ lists:map(fun ($/) ->
                                                                                  $_;
                                                                              ($\s) ->
                                                                                  $_;
                                                                              (C) ->
                                                                                  C
                                                                          end, At)]),
            spec_1(Name, SHA1, AtFilename);
        undefined ->
            {A,B,C} = now(),
            N = node(),
            TmpFile = lists:flatten(io_lib:format("/tmp/agner-~p-~p.~p.~p",[N,A,B,C])),
            Result = spec_1(Name, SHA1, TmpFile),
            file:delete(TmpFile),
            Result
    end.

spec_1(Name, SHA1, AtFilename) ->
    case file:read_file_info(AtFilename) of
        {error, enoent} ->
            {A,B,C} = now(),
            N = node(),
            TmpFile = lists:flatten(io_lib:format("/tmp/agner-~p-~p.~p.~p",[N,A,B,C])),
            ClonePort = agner_download:git(["clone", "-q", "git://github.com/" ++ proper_repo_name(Name) ++ ".git", TmpFile]),
            Result = agner_download:process_port(ClonePort, 
                                                 fun () ->
                                                         PortCheckout = agner_download:git(["checkout","-q",SHA1],[{cd, TmpFile}]),
                                                         agner_download:process_port(PortCheckout, fun () ->
                                                                                                           Config = filename:join(TmpFile, "agner.config"),
                                                                                                           {ok, S} = file:consult(Config),
                                                                                                           {ok, _} = file:copy(Config, AtFilename),
                                                                                                           S
                                                                                                   end)
                                                         end),
            os:cmd("rm -rf " ++ TmpFile),
            Result;
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
		 
