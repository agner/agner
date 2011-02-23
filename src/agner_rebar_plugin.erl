-module(agner_rebar_plugin).

-export(['get-deps'/2,
         compile/2]).


'get-deps'(_Config, AppFile) ->
    agner_main:handle_command(fetch,[{app, AppFile},{version, "@master"},{addpath, false},
                                    {install, false},{build, false}]).

compile(_Config, AppFile) ->
    agner_main:handle_command(build,[{app, AppFile},{version, "@master"},{addpath, false},
                                    {install, false}]).
