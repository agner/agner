%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_spec).
-include_lib("agner.hrl").
-export([parse/1, list_to_version/2, version_to_list/1, property_to_list/1]).

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

-spec list_to_version(agner_spec_name(), string()) -> agner_spec_version().

list_to_version(Name, "atleast:" ++ Version) ->
    case lists:reverse(lists:dropwhile(fun(V) ->
                                               V < {tag, Version}
                                       end, lists:usort(agner:versions(Name)))) of
        [] ->
            no_such_version;
        [_|_] = L ->
            hd(L)
    end;

list_to_version(_, "@" ++ Version) ->
    {flavour, Version};
list_to_version(_, Version) ->
    {release, Version}.

-spec version_to_list(agner_spec_version()) -> string().

version_to_list({flavour, Version}) ->
    "@" ++ Version;
version_to_list({release, Version}) ->
    Version.

-spec property_to_list(agner_spec_property()) -> string().
                               
property_to_list({_,V}) when is_list(V) ->
    io_lib:format("~s", [V]);
property_to_list(Prop) when is_tuple(Prop) ->
    io_lib:format("~p",[list_to_tuple(tl(tuple_to_list(Prop)))]);
property_to_list(undefined) ->
    "".

    
