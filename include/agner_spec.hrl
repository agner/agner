-type git_ref() :: sha1() |
                   {branch, string()} |
                   {tag, string()}.

-type hg_rev() :: string().

-type svn_rev() :: string().

-type agner_named_download_url() :: 
        {string(), agner_download_url()}.

-type agner_download_url() ::
        {all, list(agner_named_download_url())} |
        {git, url(), git_ref()} |
        {hg, url(), hg_rev()} |
        {svn, url(), svn_rev()}.

-type agner_install_dir() :: otp | directory().

-type agner_spec_requirement() :: {agner_package_name(), agner_package_version_string()} |
                                  agner_package_name().


-type agner_spec_property_name() :: {name, string()}.
-type agner_spec_property_description() :: {description, string()}.
-type agner_spec_property_keywords() :: {keywords, list(string())}.
-type agner_spec_property_authors() :: {authors, list(string())}.
-type agner_spec_property_homepage() :: {homepage, string()}.
-type agner_spec_property_rebar_compatible() :: {rebar_compatible, boolean()}.
-type agner_spec_property_rebar_commands() :: {rebar_commands, list(string())}.
-type agner_spec_property_build_command() :: {build_command, string()}.
-type agner_spec_property_install_command() :: {install_command, string()}.
-type agner_spec_property_install_dirs() :: {install_dirs, list(agner_install_dir())}.
-type agner_spec_property_bin_files() :: {bin_files, list(string())}.
-type agner_spec_property_license() :: {license, string(), file()} |
                                       {license, string()}.
-type agner_spec_property_erlang_versions() :: {erlang_versions, list(atom())}.
-type agner_spec_property_applications() :: {applications, list(atom())}.
-type agner_spec_property_caveats() :: {caveats, string()}.
-type agner_spec_property_url() :: {url, agner_download_url()}.
-type agner_spec_property_deps_dir() :: {deps_dir, directory()}.
-type agner_spec_property_requires() :: {requires, list(agner_spec_requirement())}.
-type agner_spec_property_code_paths() :: {code_paths, [directory()]}.
                                                             

-type agner_spec_property() :: agner_spec_property_name() |
                               agner_spec_property_description() |
                               agner_spec_property_keywords() |
                               agner_spec_property_authors() |
                               agner_spec_property_homepage() |
                               agner_spec_property_rebar_compatible() |
                               agner_spec_property_rebar_commands() |
                               agner_spec_property_build_command() |
                               agner_spec_property_install_command() |
                               agner_spec_property_bin_files() |
                               agner_spec_property_license() |
                               agner_spec_property_erlang_versions() |
                               agner_spec_property_applications() |
                               agner_spec_property_caveats() |
                               agner_spec_property_url() |
                               agner_spec_property_deps_dir() |
                               agner_spec_property_requires() |
                               agner_spec_property_code_paths().

-type agner_spec() :: list(agner_spec_property()).
