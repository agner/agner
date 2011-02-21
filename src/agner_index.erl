%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_index).
-export([behaviour_info/1]).

-spec behaviour_info(atom()) -> undefined | list({atom(), arity()}).

behaviour_info(callbacks) ->
    [{repositories,1}, {repository, 2}, {tags,2}, {branches, 2}, {spec, 3},
     {spec_url, 3}];
behaviour_info(_Other) ->
    undefined.
