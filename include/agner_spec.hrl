-type git_ref() :: sha1() |
                   {branch, string()} |
                   {tag, string()}.

-type hg_rev() :: string().

-type file() :: string().

-type agner_named_download_url() :: 
        {string(), agner_download_url()}.

-type agner_download_url() ::
        {all, list(agner_named_download_url())} |
        {git, url(), git_ref()} |
        {hg, url(), hg_rev()}.

-type agner_spec_property_name() :: {name, string()}.
-type agner_spec_property_description() :: {description, string()}.
-type agner_spec_property_keywords() :: {keywords, list(string())}.
-type agner_spec_property_authors() :: {authors, list(string())}.
-type agner_spec_property_homepage() :: {homepage, string()}.
-type agner_spec_property_rebar_compatible() :: {rebar_compatible, boolean()}.
-type agner_spec_property_build_command() :: {build_command, string()}.
-type agner_spec_property_install_command() :: {install_command, string()}.
-type agner_spec_property_bin_files() :: {bin_files, list(string())}.
-type agner_spec_property_license() :: {license, string(), file()} |
                                       {license, string()}.
-type agner_spec_property_erlang_versions() :: {erlang_versions, list(atom())}.
-type agner_spec_property_applications() :: {applications, list(atom())}.
-type agner_spec_property_caveats() :: {caveats, string()}.
-type agner_spec_property_url() :: {url, agner_download_url()}.
                                    
                                                             

-type agner_spec_property() :: agner_spec_property_name() |
                               agner_spec_property_description() |
                               agner_spec_property_keywords() |
                               agner_spec_property_authors() |
                               agner_spec_property_homepage() |
                               agner_spec_property_rebar_compatible() |
                               agner_spec_property_build_command() |
                               agner_spec_property_install_command() |
                               agner_spec_property_bin_files() |
                               agner_spec_property_license() |
                               agner_spec_property_erlang_versions() |
                               agner_spec_property_applications() |
                               agner_spec_property_caveats() |
                               agner_spec_property_url().

-type agner_spec() :: list(agner_spec_property()).
