-module(agner_github, [Account]).
-behaviour(agner_index).
-include_lib("agner.hrl").
-include_lib("agner_index.hrl").

-export([repositories/0,
		 repository/1,
		 tags/1,
		 branches/1,
		 spec/2
		]).

repositories() ->
	case request("https://github.com/api/v2/json/repos/show/" ++ Account)  of
		{error, _Reason} = Error ->
			Error;
		{struct, Object} ->
			Repositories = proplists:get_value(<<"repositories">>, Object),
			lists:filter(fun (invalid) ->
							  false;
						  (_) ->
							  true
					  end,
					  lists:map(fun ({struct, RepObject}) ->
										repo_name(proplists:get_value(<<"name">>, RepObject))
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
	case request("http://github.com/api/v2/json/repos/show/" ++ proper_repo_name(Name) ++  "/tags") of
		{struct, Object} ->
			{struct, Tags} = proplists:get_value(<<"tags">>, Object),
			lists:map(fun({Tag, SHA1}) ->
							  {binary_to_list(Tag),
							   binary_to_list(SHA1)}
					  end, Tags)
	end.


branches(Name) ->
	case request("http://github.com/api/v2/json/repos/show/" ++ proper_repo_name(Name) ++  "/branches") of
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
	case httpc_request("http://github.com/" ++ proper_repo_name(Name) ++ "/raw/" ++ SHA1 ++ "/agner.config") of
        {ok, {{"HTTP/1.1",404,_},_Headers,_Body}} ->
            {error, not_found};
        {ok, {{"HTTP/1.1",200,_},_Headers,S}} ->
            agner_spec:parse(S)
	end.


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
	{error, not_found}.

%%%

repo_name(B) when is_binary(B) ->
	S = binary_to_list(B),
	case string:tokens(S,".") of
		[Name,"agner"] ->
			Name;
		_ ->
			invalid
	end.
		 
