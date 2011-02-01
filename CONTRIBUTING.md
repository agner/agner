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

### Build command

If your target repository can't be built by simply invoking `rebar get-deps && rebar compile` then you need to supply
a `build_command` property, for example:

     {build_command, "make"}.

Alternatively, you can also use it to finalize build process after rebar get-deps & compile for rebar compatible projects.

The build command will be invoked in checked out directory containing target repository.

### Install command

If your target repository can't be built by simply invoking `rebar get-deps && rebar compile` then you need to supply
a `install_command` property, for example:


     {install_command, "make install"}.

The install command will be invoked in checked out directory containing target repository. 


### Environment Variables

Here's the current list of environmentvariables that will be available to build & install commands:

* AGNER -- Absolute path to the agner script file
* AGNER_PREFIX -- Root agner's directory (defaults to `/usr/local/agner`)
* AGNER_PACKAGE_REPO -- Path to checked out .agner repo

Here's the current list of environmentvariables that will be available to install command:

* AGNER_INSTALL_PREFIX -- A place where install procedure should consider putting installable files to. Right now it equates to
  `AGNER_PREFIX/agner/packages/<package_name>-<package_version>`

More variables to come later.

### Private Branches & Tags

If you want to have either branches or tags that shouldn't be exposed to the end user as versions when they inquire using
`agner versions`, simply prepend your branch or tag name with `%` (comment) symbol and it will be hidden from the general public.
It will still be possible to use such versions explicitly, though (for example, `agner spec erlv8 -v @%test` for a branch called `%test`)

## Tips & Tricks

### Rewriting multiple versions

Ever found yourself in a situation when you made a typo across multiple versions of the specification? I know I did, multiple
times. So here's a quick-n-dirty solution for this.

Let's assume you have a branch named `release` (@release flavour) and some release versions that you want to rewrite; and you need to change misspelled `nae` property to `name`.

What you can do is:

1. Create new `release2` branch

        git checkout -b release2

2. Do the actual replacement

        git tag -l | sort | xargs -I {} sh -c 'git cherry-pick -n {} && cat agner.config | sed s/nae/name/g > agner.new && mv agner.new agner.config && git add agner.config && git commit -C {} && git tag -d {} && git tag {}'

3. Remove remote `release branch`

        git push origin :release

4. Push `remote2` as `remote`

        git push origin release2:release

5. Remove remote tags:

        git tag -l | xargs -I {} git push origin :{}

6. Push tags:

        git push origin --tags

P.S. If you want to stick to proper release version tags only (for example, if you use hidden % tags), you might want to use `agner versions PACKAGE --no-flavours` command instead of `git tag -l`