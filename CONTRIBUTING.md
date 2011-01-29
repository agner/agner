CONTRIBUTING
============

How can I suggest a new package?
--------------------------------

Just [open an issue](https://github.com/agner/agner/issues) with your suggestion.

How can I create a new package?
-------------------------------

You can create a spec repository by forking and renaming [template package repo](https://github.com/agner/agner.template)
and later on submitting it for inclusion by [opening an issue](https://github.com/agner/agner/issues). 
The last step is only necessary if you want this package to be published on a "central" index. 
If you want to keep it semi-private, you can just use it from your own index.


Mastering specification
-----------------------

First of all, please take some time to read through [README](README.md) to make sure you understand all the concepts,
especially how versioning is done.

Also, even though in Git world you can be crucified for deleting remote tags (`release` versions in our lingo), in Agner,
even not encouraged, this type of action is tolerable if you need to change the specification of an already published release. 
`.agner` repos are intended for quick fresh cloning so shuffling tags in them is not that critical. If you do anticiapte anticipate that somebody has a fork of your `.agner` repo, though, please communicate your tag changes to them, just in case. One of
the ways might be adding some README or NOTES file into your `.agner` repo.

Before commiting your agner.config, please make sure you ran `agner verify` on it so it passes at least
some validity checks.

Build command
~~~~~~~~~~~~~

If your target repository can't be built by simply invoking `rebar get-deps && rebar compile` then you need to supply
a `build_command` property, for example:


     {build_command, "make"}.

The build command will be invoked in checked out directory containing target repository.

Install command
~~~~~~~~~~~~~~~

If your target repository can't be built by simply invoking `rebar get-deps && rebar compile` then you need to supply
a `install_command` property, for example:


     {install_command, "make install"}.

The install command will be invoked in checked out directory containing target repository. 


Environment Variables
~~~~~~~~~~~~~~~~~~~~~

Here's the current list of environmentvariables that will be available to build & install commands:

* AGNER_PREFIX -- A place where install procedure should consider putting installable files to. Right now it defaults to
  `/usr/local` but eventually will point to a properly isolated deployment environment

More variables to come later.
