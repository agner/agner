-type git_ref() :: sha1() |
                   {branch, string()} |
                   {tag, string()}.

-type hg_rev() :: string().

-type file() :: string().

-type agner_spec_property_name() :: {name, string()}.
-type agner_spec_property_description() :: {description, string()}.
-type agner_spec_property_authors() :: {authors, list(string())}.
-type agner_spec_property_homepage() :: {homepage, string()}.
-type agner_spec_property_rebar_compatible() :: {rebar_compatible, boolean()}.
-type agner_spec_property_license() :: {license, string(), file()} |
                                       {license, string()}.
-type agner_spec_property_erlang_versions() :: {erlang_versions, list(atom())}.
-type agner_spec_property_url() :: {url, 
                                    {git, url(), git_ref()} |
                                    {hg, url(), hg_rev()}}.
                                    
                                                             

-type agner_spec_property() :: agner_spec_property_name() |
                               agner_spec_property_description() |
                               agner_spec_property_authors() |
                               agner_spec_property_homepage() |
                               agner_spec_property_rebar_compatible() |
                               agner_spec_property_license() |
                               agner_spec_property_erlang_versions() |
                               agner_spec_property_url().

-type agner_spec() :: list(agner_spec_property()).
