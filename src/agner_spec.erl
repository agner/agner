-module(agner_spec).
-include_lib("agner.hrl").
-export([parse/1]).

-type agner_spec_source() :: string().

-spec parse(agner_spec_source()) -> agner_spec().
                   
parse(S) ->
    {ok, Tokens, _ } = erl_scan:string(S),
    lists:reverse(
      lists:map(fun (Term) ->
                        {ok, T} = erl_parse:parse_term(lists:reverse(Term)),
                        T
                end,
                tl(lists:foldl(fun ({dot, _}=Dot,[Term|Terms]) ->
                                       [[]|[[Dot|Term]|Terms]];
                                   (Token, [Term|Terms]) ->
                                       [[Token|Term]|Terms];
                                   (Token, []) ->
                                       [[Token]]
                               end, [], Tokens)))).
