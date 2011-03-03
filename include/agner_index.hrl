%% depends on include/agner.hrl
-spec repositories(agner_account()) -> list(agner_repo()) | not_found_error().
-spec tags(agner_account(), agner_repo()) -> list({agner_repo_tag(),sha1()}) | not_found_error().
-spec branches(agner_account(), agner_repo()) -> list({agner_repo_branch(), sha1()}) | not_found_error().
-spec spec(agner_account(), agner_repo(), agner_package_version()) -> agner_spec() | not_found_error().
-spec spec_url(agner_account(), agner_repo(), sha1()) -> url() | not_found_error().
-spec exists(agner_account(), agner_repo()) -> true | false.
                    
