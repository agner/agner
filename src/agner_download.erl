-module(agner_download).
-export([fetch/2]).

fetch({git, URL, Ref}, Directory) ->
    PortClone = git(["clone", URL, Directory]),
    process_port(PortClone, fun () ->
                                    PortCheckout = git(["checkout",Ref],[{cd, Directory}]),
                                    process_port(PortCheckout, fun () -> ok end)
                            end).

%%

git(Args) ->
    git(Args,[]).

git(Args, Opts) ->
    Git = os:find_executable("git"),
    open_port({spawn_executable, Git},[{args, Args},
                                       exit_status|Opts]).

process_port(Port, Fun) ->
    receive 
        {Port, {exit_status, 0}} ->
            apply(Fun, []);
        {Port, {exit_status, Status}} ->
            {error, Status};
        {'EXIT', Port, PosixCode} ->
            {error, PosixCode}
    end.
