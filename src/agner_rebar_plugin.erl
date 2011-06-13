-module(agner_rebar_plugin).

-export(['check-deps'/2,
         'get-deps'/2,
         pre_compile/2]).


'check-deps'(Config, AppFile) ->
    pre_compile(Config, AppFile).

'get-deps'(Config, AppFile) ->
    agner:start(),
    set_indices(Config),
    agner_main:handle_command(fetch,[{app, AppFile},{version, "@master"},{addpath, false},
                                     {install, false},{build, false}]),
    agner:stop().

pre_compile(Config, AppFile) ->
    agner:start(),
    set_indices(Config),
    agner_main:handle_command(build,[{app, AppFile},{version, "@master"},{quiet, false},
                                     {nofetch, true},
                                     {addpath, false},
                                     {install, false}]),
    agner:stop().

set_indices(Config) ->
        application:set_env(agner, indices, rebar_config:get_local(Config, agner_indices, [{github,"agner"}])).
