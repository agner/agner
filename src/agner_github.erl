-module(agner_github, [Account]).
-export([list_all_repos/0,
		 list_tags/1,
		 list_branches/1,
		 blob/3
		]).

list_all_repos() ->
	request("https://github.com/api/v2/json/repos/show/" ++ Account).
	
list_tags(Name) ->
	request("http://github.com/api/v2/json/repos/show/" ++ Account ++ "/" ++ Name ++  "/tags").

list_branches(Name) ->
	request("http://github.com/api/v2/json/repos/show/" ++ Account ++ "/" ++ Name ++  "/branches").

blob(Name, SHA1, Path) ->
	request("http://github.com/api/v2/json/blob/show/" ++ Account ++  "/" ++ Name ++  "/" ++ SHA1 ++ "/" ++ Path).

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
parse_response({ok, {{"HTTP/1.1",200,_},_Header,Body}}) ->
	mochijson2:decode(Body).
