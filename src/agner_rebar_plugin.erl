-module(agner_rebar_plugin).

-export(['check-deps'/2,
         'get-deps'/2,
         pre_compile/2]).


'check-deps'(Config, AppFile) ->
    pre_compile(Config, AppFile).

'get-deps'(Config, AppFile) ->
    ensure_agner_started(),
    set_indices(Config),
    agner_main:handle_command(fetch,[{app, AppFile},{version, "@master"},{addpath, false},
                                     {install, false},{build, false}]).

pre_compile(Config, AppFile) ->
    ensure_agner_started(),
    set_indices(Config),
    agner_main:handle_command(build,[{app, AppFile},{version, "@master"},{quiet, false},
                                     {nofetch, true},
                                     {addpath, false},
                                     {install, false}]).

set_indices(Config) ->
        application:set_env(agner, indices, rebar_config:get_local(Config, agner_indices, [{github,"agner"}])).

ensure_agner_started() ->
    case proplists:get_value(agner, application:loaded_applications()) of
        undefined ->
            agner:start();
        _ ->
            ok %% already started
    end.
            
