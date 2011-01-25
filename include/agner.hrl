-type agner_spec_name() :: string() | atom().
-type agner_spec_version() :: string().

-type agner_key() :: term().
-type agner_value() :: term().
-type agner_spec() :: list({agner_key(), agner_value()}).

-type agner_repo() :: string().
-type agner_repo_tag() :: string().
-type agner_repo_branch() :: string().

-type sha1() :: string().

-type not_found_error() :: {error, not_found}.
