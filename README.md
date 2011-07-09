Agner
=====

Agner is a rebar-friendly Erlang package index inspired by Clojars and
Homebrew.

Essentially, Agner is an index of Erlang packages with some extra
capabilities such as versioning, downloads, installation and so on.

Agner is a shorthand for *A Giant Nebula of Erlang Repositories*. It
also pays homage to the Danish statistician Agner Krarup Erlang.

Installation
------------

It is just a matter of a simple oneliner to get Agner installed:

     curl https://raw.github.com/agner/agner/master/scripts/oneliner | sh

If you want to install latest release instead of HEAD, use this one:

     curl https://raw.github.com/agner/agner/master/scripts/oneliner.release | sh

Motivation
----------

By now, there is a large set of Erlang tools and libraries available,
all of them highly useful. The problem, however, is to provide an index
of these packages, so other people

   * Know of their existence
   * Can easily use a package in their own projects
   * Can search for a particular package

Agner aims to provide such an index, by focusing on a number of
points:

* The index is loose in the sense that anyone can overlay the index
  and add their own packages to the repository
* The tool is as simple as possible, utilizing git as a backend (for the time
  being) to maintain the indices
* Recognize the ideas of simplicity Joe Armstrong had in mind on
  the erlang-questions@ mailing list on the
  [22th of July 2010](http://www.erlang.org/cgi-bin/ezmlm-cgi?4:mss:52415:201007:npnohnblfemjooohecnk)

Use
===

This section introduces the terminology of Agner:

* **index/indices:** Where Agner finds its index of available of
  available packages. Usually this is a github user with one or more
  Agner-packages among the users git repositories. You can enable
  multiple indices at the same time if you want.
* **package:** A separate library or program indentified by the
  index. It is an `.agner` repository underneath the index github
  user, so one example would be `agner/gproc.agner` specifying a
  package for the `gproc` library undernath the `agner`-user. A
  package may be overridden by an index; a very important feature that
  allows installing packages not hosted in a central index.
* **project:** A software project, program or library, containing
  the actual source code for the program or library. In the running example,
  this is `esl/gproc` on github.
* **release:** A release of a package signifying a point in time
  where the package was deemed to be in a certain (stable) state. Is usually
  used when a new version of the software is released to the
  general public so you can refer to package X version Y
* **flavour:** A moving target of a package with some specified
  behaviour. It is used for tracking the development of a package
  over time. Common flavours include the *@master* flavour, used to
  track the development branch of a package and the *@release*
  flavour, used to track the latest release of the package.

Command invocation
------------------

    agner help [COMMAND]

Print a command overview on the command line. If the optional
COMMAND is given, show help for the given command.

### Listing, searching:

    agner list [-d/--descriptions] [-p/--properties PROPERTY1[,PROPERTY2]...]
               [-s/--search SEARCH_TERM]

Will list all Agner packages. It is used for quick overviews of the
packages that are available in the enabled indices for installation or
use in a program. This command, like most commands, accepts command
line flags:

* `-d` or `--descriptions`: With this option enabled, Agner will also
print out the descriptions of the packages, for easy grepping to find
relevant packages. It accepts the following flags:

* `-p` or `--properties`: A comma, separated list of properties to be
included in the listing (when present).

* `-s` or `--search`: The packages name, description and keyword are
searched, matched against `SEARCH_TERM`. Only matching items are shown.

Since searching for packages is so common, a convenience command has
been defined:

    agner search SEARCH_TERM [-d/--description] [-p/--properties PROPERTY1,PROPERTY2]

This is an alias for `agner list -s`

### Printing package specifications (spec's)

    agner spec PACKAGE [-v/--version VERSION] [-b/--browser]
                       [-h/--homepage] [-p/--property PROPERTY]
                       [-s/--spec-file SPECFILE]

Will print a specification of a given package on stdout. The specification is the information Agner uses to determine the location of the package as well as the relevant meta-data for the package. Flags:

* `-v` or `--version`: If this version constraint option is given,
Agner will print out the specification for that given VERSION (for
example `agner spec gproc -v @release`). The version can be either a *release* or a *flavour*. By default, the `@master` flavour is chosen.

* `-b` or `--browser`: Open a browser with the specification file in
its respective `.agner` repository at github.

* `-h` or `--homepage`: Open a browser with the package's homepage.

* `-p` or `--property`: Agner will only render a particular PROPERTY value
instead of a full specification (example: `agner spec -p rebar_compatible yaws`).

* `-s` or `--spec-file`: A flag primarily intended for package
maintainers. This way they can specify their local `agner.config`
files to test their package.

### Fetching, building and installing

    agner fetch PACKAGE [DESTDIR] [-v/--version VERSION] [-b/--build]
                                  [-a/--add-path] [-i/--install]
                                  [-s/--spec-file SPECFILE] [--package-path PACKAGEPATH]
                                  [-q/--quiet]

Fetch a given PACKAGE to either the current directory or,
optionally, to the DESTDIR directory. Flags:

* `-v` or `--version`: The version constraint is as is the case for
`agner spec`. You can choose to constrain a fetch to a given
VERSION, either a *relase* or a *flavour*.

* `-b` or `--build`: Agner will try to build fetched package. Only
rebar-compatible packages or packages with `build_command` can be
built. If you also specify `-a` (or `--add-path`) Agner will add path
to a newly built package to your `$HOME/.erlang`

* `-i` or `--install`: If the package has the `install_command`
property defined, Agner will also install this package. Please note
that in most cases you should also specify `-b` (or `--build`) in order for
installation to make sense.

* `-s` or `--spec-file`: A flag primarily intended for package
maintainers. This way they can specify their local `agner.config`
files to test their package. Can be used in conjunction with
`--package-path` to point to a checkout copy of an `.agner` repo (will
be used to set `$AGNER_PACKAGE_REPO` variable for shell commands,
defaults to `.`)

* `-q` or `--quiet`: A flag that will suppress output of `build_command` & `install_command` 
(if present)

For convenience, the following shortcuts are defined:

    agner build PACKAGE [DESTDIR] [-v/--version VERSION] [-s/--spec-file SPECFILE] [-a/--add-path] [-i/--install] [--no-fetch]

Alias for `agner fetch --build PACKAGE`.

    agner install PACKAGE [-v/--version VERSION] [-s/--spec-file SPECFILE] [--no-fetch]

Alias for `agner fetch --build --install PACKAGE
/tmp/<uniq_filename>`. A typical example would be `agner install
rebar` or `agner install rebar -v @agner` to get `rebar` binary in
your `PATH`. It is assumed that `install_command` property will make use
of `AGNER_INSTALL_PREFIX` OS environment variable.

`--no-fetch` option could be useful when you don't want to update already 
fetched repository, but just build or install it.

### Uninstalling

    agner uninstall PACKAGE [-v/--version VERSION] [-s/--spec-file SPECFILE]

Uninstall given package (and a particular VERSION of it, if specified
- see `agner spec`). Will use local SPECFILE if the `-s` (or `--spec-file`)
option is passed.

### Query

    agner versions PACKAGE [--no-flavours] [--no-releases]

List the versions of the given PACKAGE. Specifying `--no-flavours`
will omit flavour versions; and specifying `--no-releases` will omit
release versions respectively.

    agner prefix PACKAGE [-v/--version VERSION]

Prints prefix where PACKAGE is installed. If package is not installed,
prints nothing.

    agner config [VARIABLE]

Shows main environmental variables. If VARIABLE is omitted, then lists
`key=value` for each variable. If not omitted, prints just its
value. Currently supported variables are: `prefix` and `bin`.

### Packaging

    agner create PACKAGE [--github-account ACCOUNT]

Contributor's tool that clones `.agner` repo template and sets its
origin to ACCOUNT (by default, equals `agner`, so if you don't have a
permission to create repos in `agner`, set --github-account to your
personal or organization account.

    agner verify [SPECFILE]

Verify SPECFILE as a specification file for correctness; intended to
be used to package maintainers to simplify their workflow. If SPECFILE
is not given, it defaults to `agner.config`. Currently checks whether
1) specification is a valid file that can be parsed, 2) the URL can be
fetched. In the future it will also offer a deeper analysis of
specification correctness.

Packaging
=========

Package organization
--------------------

When Agner is invoked, it will scan its *indices* for package
lists. The default index is "agner", which is located at
[https://github.com/agner/](https://github.com/agner/). The index is
scanned by looking for *Agner repositories* which are normal (github)
repositories suffixed with `.agner`. An example is the repository
[https://github.com/agner/getopt.agner](https://github.com/agner/getopt.agner)
which contains the package details of the `getopt` package.

It is important to nail down that there are three balls in the air:

* The index user, who has a list of
* `.agner` repositories, which points to
* Erlang software projects

By making a split between the repository containing the project and
the repository containing the package, we make it easy to identify
`.agner` repositories, and we enable a simple way to make the project
live in another source control system, for instance Mercurial (hg). It
is also way easier to keep the (small) `.agner` repositories in an
index and in the long run, it provisions for local caching.

Further indices can be added to Agner through the environment (TODO:
flesh out how that is done). Indices are searched
in the order of specification, allowing for overriding of a given
index. This allows you to create local indices or special indices for
your own use, or try something out on top of other indices.

The multiple indices approach solves authorization questions "the git
way": you put trust in the indices you add to Agner, so if you don't
trust an index, you can simply refrain from adding it. The main
"agner" index is intended to be the official source, but we recognize
that individuals might have reasons to overlay another index on
top. By having a loose index-construction, we hope to alleviate some
of the problems with access rights.

Package names
-------------

Packages are named in one of two ways. Either as the a direct name,
<code>mochiweb</code>, or as a form with a prefix of the package
index, <code>account/package</code>. For example
<code>yrashk/misultin</code>). We use package names to identify a
given package in Agner - but versions of the package is naturally not
part of its name. This allows for packages to exist in multiple
versions at the same time.

Versions
--------

Agner has two kinds of versions:

* Release versions, normally something like <code>1.2.0</code>,
  represented using tags in `.agner` repos.
* Flavour versions, normally something like <code>@release</code>,
  represented using branches in `.agner` repos. Note the prefix of "@"
  which is present on flavours only.

The intention is that a *release* version marks a given point in time
where a given version of the code base was released to the general
public. When Erlang/OTP is released as OTP-R14B01 for instance, it
signifies a *release* in Agner-terminology. On the other hand, a
*flavour* signifies a moving target. Continuing the OTP-R14B01
example from before, it would be natural to have a *@dev* flavour
which tracks the Erlang/OTP branch called `dev`. The other important
flavour is *@release* which will track the latest release.

Also, the command line utility and Agner-enabled rebar will recognize
`atleast:VERSION` format (for example, `atleast:1.5.0`) and will use
the latest version after VERSION (so, if some package already has a
version of `1.6`, `atleast:1.5.0` will select `1.6`. This is mostly
for scenarios when `@release` flavour is absent or broken.

### How to create relases and flavours

As hinted, a release version is a *tag* in a `.agner` repository. So
to create a release, you alter the `.agner` repository to match your
liking and then you tag it (with a standard `git tag` command
invocation). Agner will now pick up the change.

Likewise, for a flavour version, you *branch* the `.agner` repository
and alter the branch so it does what your flavour intended to
do. Flavours can be made for anything you would like to track over
time. By default, the advice is to create two flavours, *@master* and
*@release*. These tracks, respectively, the current development of a
project and the latest stable release of that project.

Keeping everything up-to-date is outsourced to git and you can use
usual git-commands to manipulate the `.agner` repository.

### The contents of an .agner package

The `.agner` package repository contains a file of Erlang-terms, called
`agner.config`. This file looks like this:

    {name, "etorrent"}.
    {authors, ["Jesper Louis Andersen <jesper.louis.andersen@gmail.com>"]}.
    {description, "Etorrent is a bittorrent client implementation in Erlang focusing on fault-tolerance"}.
    {homepage, "http://github.com/jlouis/etorrent"}.
    {rebar_compatible, true}.
    {license, "BSD2", "COPYING"}.
    {erlang_versions, [otp_r14b, otp_r14b01, otp_r13b04]}.
    {url, {git, "https://github.com/jlouis/etorrent.git", {branch, "master"}}}.

Or in a more generic way:

    {name, ProjectName}.
    {authors, [Author]}.
    {description, ProjectDescription}.
    {homepage, ProjectHomepage}.
    {rebar_compatible, IsRebarCompatible}.
    {license, LicenseType [, LicenseFile]}.
    {erlang_versions, [OTPAtom]}.
    {url, UrlSpec}.

The meaning of the individual fields are as in the following:

* `ProjectName :: string()` - is the project name. This is usually
  named the same as the `.agner` package to minimize confusion.

* `[Author] :: [string()]` - Can really be any string, but it is
  usually the names of the project authors in a list including their
  email-addresses for easy contact.

* `ProjectDescription :: string()` - A description of the
  project. Used for searching through projects.

* `ProjectHomepage :: string()` - The URL of the homepage of the
  project.

* `IsRebarCompatible :: boolean()` - Set to `true` if this project
  uses `rebar` or is compilable by rebar even if it wasn't originally designed for that.

* `LicenseType :: string(), LicenseFile :: string()` - Two
  strings. The first one specifies the general license type of the
  project and the second string explains where the license is to be
  found from the top level directory (usually file-names like
  `COPYING` or `LICENSE` are used for this). Please note that `LicenseFile` is optional.

* `[OTPAtom] :: [otp_rXXb | otp_rXXbYY]` - A list of what OTP versions
  the project can be used with. the `XX` is a major release number in
  Erlang/OTP (12,13,14,...) and `YY` is a minor release number (01,
  02, ...).

* `UrlSpec :: {git, URL, GitSpec}` - Specifies where to fetch the
  project. `GitSpec` has type `sha1() | {tag, string()} | {branch, string()}`
  and points to either string-based sha1 representation, a git *tag* or a *git* branch
  respectively. Notice that you can't specify more than one target in
  this file. To handle multiple versions, you use *releases* and
  *flavours* by altering the `.agner` repository wherein this
  configuration file lies.

* `UrlSpec :: {hg, URL, HgRev}` - Specifies where to fetch the
  project. `HgSpec` has type `string()` and  points to either string-based revision representation

The very latest specification typespecs are available in [agner_spec.hrl](agner/tree/master/include/agner_spec.hrl)

It is **highly recommended** that `.agner` repo maintainers use `agner verify` command before
committing and pushing their updated specifications.


Rebar
-----

Agner-compatible rebar can be installed by `agner install rebar`. 

Using it with rebar is fairly simple, it makes use of rebar's plugin system. 
Just include our plugin in your rebar.config and reference default `deps` directory:

    {rebar_plugins, [agner_rebar_plugin]}.
    {lib_dirs, ["deps"]}.

then, in your .app or .app.src file, add something like this as one of your `application keys`:

     ...
     {agner, [
           {requires, ["typespecs","getopt","gproc","plists","gen_fsm2","jsx","rebar"]}
          ]}
     ...

An example can be found in [agner itself](https://github.com/agner/agner/blob/master/src/agner.app.src)

Also, you can also specify your own indices in rebar.config

    {agner_indices, [{github, "yourgithubusername"},{github,"agner"}].

Contributing
------------

If you want to contrinute to Agner, please read the document at
[CONTRIBUTING](agner/tree/master/CONTRIBUTING.md).






