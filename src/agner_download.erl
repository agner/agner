%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_download).
-export([fetch/2]).
%% internal exports
-export([git/1, git/2, process_port/2]).

fetch(Spec, Directory) ->
    Fetch = fetch_1(proplists:get_value(url, Spec), Directory),
    {ok, F} = file:open(filename:join(Directory,".agner.config"),[write]),
    lists:foreach(fun (Term) ->
                          io:fwrite(F,"~p.~n",[Term])
                  end, Spec),
    file:close(F),
    Fetch.

fetch_1({all, []}, _) ->
    ok;

fetch_1({all, [{Name, URL}|Rest]}, Directory) ->
    fetch_1(URL, filename:join(Directory, Name)),
    fetch_1({all, Rest}, Directory);

fetch_1({git, URL, Ref}, Directory) ->
    case filelib:is_dir(Directory) of
        false -> %% clone
            PortClone = git(["clone", URL, Directory]),
            process_port(PortClone, fun () -> git_checkout(Ref, Directory) end);
        true -> %% existing repo (or something else)
            PortFetch = git(["fetch","origin"], [{cd, Directory}]),
            process_port(PortFetch, fun() -> git_checkout(Ref, Directory) end)
    end;

fetch_1({hg, URL, Rev}, Directory) ->
    case filelib:is_dir(Directory) of
        false -> %% new
            PortClone = hg(["clone", "-U", URL, Directory]),
            process_port(PortClone, fun () -> 
                                            PortUpdate = hg(["update", Rev]),
                                            process_port(PortUpdate, fun () -> ok end)
                                    end);
        true -> %% existing
            PortClone = hg(["pull", "-u", "-r", Rev],[{cd, Directory}]),
            process_port(PortClone, fun () -> ok end)
    end.

%%

git_checkout({branch, Ref}, Directory) when is_list(Ref)->
    PortCheckout = git(["checkout","-q","origin/" ++ Ref],[{cd, Directory}]),
    process_port(PortCheckout, fun () -> ok end);
git_checkout({tag, Ref}, Directory) when is_list(Ref) ->
    PortCheckout = git(["checkout","-q",Ref],[{cd, Directory}]),
    process_port(PortCheckout, fun () -> ok end);
git_checkout(Ref, Directory) when is_list(Ref) ->
    PortCheckout = git(["checkout","-q",Ref],[{cd, Directory}]),
    process_port(PortCheckout, fun () -> ok end).
    
git(Args) ->
    git(Args,[]).

git(Args, Opts) ->
    Git = os:find_executable("git"),
    open_port({spawn_executable, Git},[{args, Args}, stderr_to_stdout,
                                       exit_status|Opts]).

hg(Args) ->
    hg(Args,[]).

hg(Args, Opts) ->
    Hg = os:find_executable("hg"),
    open_port({spawn_executable, Hg},[{args, Args},
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
