-type agner_spec_name() :: string() | atom().
-type agner_spec_version() :: {branch, string()} |
                              {tag, string()}.
-type agner_spec_version_string() :: string().

-type agner_repo() :: string().
-type agner_repo_tag() :: string().
-type agner_repo_branch() :: string().

-type sha1() :: string().

-type agner_index() :: {github, string()}.
-type agner_indices() :: list(agner_index()).

-type not_found_error() :: {error, not_found}.

-type directory() :: string().

-type url() :: string().

-include_lib("agner_spec.hrl").
