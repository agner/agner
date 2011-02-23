-module(agner_rebar_plugin).

-export(['get-deps'/2,
         compile/2]).


'get-deps'(Config, AppFile) ->
    agner:start(),
    set_indices(Config),
    agner_main:handle_command(fetch,[{app, AppFile},{version, "@master"},{addpath, false},
                                     {install, false},{build, false}]),
    agner:stop().

compile(Config, AppFile) ->
    agner:start(),
    set_indices(Config),
    agner_main:handle_command(build,[{app, AppFile},{version, "@master"},{quiet, false},
                                     {addpath, false},
                                     {install, false}]),
    agner:stop().

set_indices(Config) ->
        application:set_env(agner, indices, rebar_config:get_local(Config, agner_indices, [{github,"agner"}])).
