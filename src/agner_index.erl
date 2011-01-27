-module(agner_index).
-export([behaviour_info/1]).

-spec behaviour_info(atom()) -> undefined | list({atom(), arity()}).

behaviour_info(callbacks) ->
    [{repositories,0}, {repository, 1}, {tags,1}, {branches, 1}, {spec, 2},
     {spec_url, 2}];
behaviour_info(_Other) ->
    undefined.

