%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_spec).
-include_lib("agner.hrl").
-export([parse/1, list_to_version/2, version_to_list/1, property_to_list/1, version_compare/3, normalize/1]).

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

-spec list_to_version(agner_package_name(), string()) -> agner_package_version().

list_to_version(Name, "atleast:" ++ Version) ->
    case lists:reverse(lists:dropwhile(fun({release,V}) ->
                                               version_compare('<',V,Version);
                                          (_) ->
                                               true
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

-spec version_to_list(agner_package_version()) -> string().

version_to_list(no_such_version) ->
    undefined;
version_to_list({flavour, Version}) ->
    "@" ++ Version;
version_to_list({release, Version}) ->
    Version.

-spec property_to_list(agner_spec_property()) -> string().
                               
property_to_list(Prop) when is_tuple(Prop) ->
    property_to_list(tuple_to_list(Prop));
property_to_list([_Name|Rest]) when is_list(Rest) ->
    [ property_to_list_1(V) || V <- Rest ];
property_to_list(undefined) ->
    "".

property_to_list_1(V) when is_list(V) ->
    case io_lib:printable_list(V) of
        true ->
            io_lib:format("~s ", [V]);
        false ->
            [ property_to_list_1(V1) || V1 <- V ]
    end;
property_to_list_1(V) ->
    io_lib:format("~p ", [V]).
    

    

-spec version_compare('>'|'<'|'>='|'=<'|'=='|'=:='|'/='|'=/=', agner_package_version_string(), agner_package_version_string()) -> boolean().

version_compare(Op, Version1, Version2) ->
    erlang:apply(erlang, Op, [version_components(Version1), version_components(Version2)]).

version_components(Version) ->
    Components = string:tokens(Version, "."),
    lists:flatten(lists:map(fun (C) ->
                                    case string:to_integer(C) of
                                        {error, _} = Error ->
                                            error(Error);                                        
                                        {I, Rest} ->
                                            [I, Rest]
                                    end
                            end, Components)).
                              

-spec normalize(agner_spec()) -> agner_spec().

normalize(Spec) ->    
    lists:ukeysort(1,Spec ++ defaults(proplists:get_value(name, Spec, ""))).

-spec defaults(agner_package_name()) -> agner_spec().

defaults(Package) ->                              
    [{rebar_compatible, false},
     {requires, []},
     {deps_dir, "deps"},
     {rebar_commands, ["get-deps","compile"]},
     {homepage, "http://google.com/#q=" ++ Package},
     {description, ""},
     {keywords, []},
     {code_paths, ["ebin"]}
     {applications, []}].
                             
