Agner
=====

Agner is a rebar-friendly Erlang package index inspired by Clojars and Homebrew.

Package names
-------------

Package name is just either a package name such as <code>mochiweb</code>, or (in case of github indeces, it might also
take a form of <code>account/package</code>, for example <code>yrashk/misultin</code>)

Versions
--------

Agner has two kinds of versions:

* Release versions, normally something like <code>1.2.0</code>, represented using tags in .agner repos
* Flavour versions, normally something like <code>@release</code>, represented using branches in .agner repos

Commands
--------

    agner list [-d/--descriptions]

    agner spec [package name] [-v/--version package_version]

    agner fetch [package name] [destination directory] [-v/--version package_version]

    agner versions [package name]


Rebar
-----

Agner-compatible rebar is available at (agner branch)[https://github.com/agner/rebar/tree/agner] of (agner/rebar)[https://github.com/agner/rebar]. Or you can download ready-made rebar from https://github.com/agner/agner/raw/master/rebar.

Using it with rebar is fairly simple, it uses rebar's deps feature:

    {deps, [
              {typespecs, "0.1", {agner, "typespecs"}},
              {getopt, "0.3.0", {agner, "getopt"}}
           ]}.

You can also specify your own indeces:

    {agner_indices, [{github, "yourgithubusername"},{github,"agner"}].

