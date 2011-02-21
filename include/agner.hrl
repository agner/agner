-type agner_package_name() :: string() | atom().
-type agner_package_version() :: {flavour, string()} |
                              {release, string()}.
-type agner_package_version_string() :: string().

-type agner_repo() :: string().
-type agner_repo_tag() :: string().
-type agner_repo_branch() :: string().

-type sha1() :: string().

-type agner_index() :: github.
-type agner_account() :: string().
-type agner_indices() :: list({agner_index(), agner_account()}).

-type not_found_error() :: {error, not_found}.

-type directory() :: string().

-type url() :: string().

-type file() :: string().

-include_lib("agner_spec.hrl").
