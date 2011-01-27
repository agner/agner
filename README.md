Agner
=====

Agner is a rebar-friendly Erlang package index inspired by Clojars and Homebrew.

Rebar
-----

Agner-compatible rebar is available at (agner branch)[https://github.com/agner/rebar/tree/agner] of (agner/rebar)[https://github.com/agner/rebar]. Or you can download ready-made rebar from https://github.com/agner/agner/raw/master/rebar.

Using it with rebar is fairly simple, it uses rebar's deps feature:

```
{deps, [
        {typespecs, "0.1", {agner, "typespecs"}},
        {getopt, "0.3.0", {agner, "getopt"}}
        ]}.
```

You can also specify your own indeces:

```
{agner_indices, [{github, "yourgithubusername"},{github,"agner"}].
```

