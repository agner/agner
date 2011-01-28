%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_utils).
-export([launch_browser/1]).

launch_browser(URL) ->
    case os:type() of
        {unix, darwin} ->
            os:cmd("open " ++ URL);
        {unix, linux} ->
            os:cmd("firefox " ++ URL ++ " &")
    end.

