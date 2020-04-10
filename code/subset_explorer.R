search_res <- readr::read_csv(here::here("data", "explorer02.csv"))

headlines_lower <- stringr::str_to_lower(search_res$title)

covid_related <- stringr::str_detect(headlines_lower, "(coronavirus)|(covid)|(sars)|(cov)")
jail_related <- stringr::str_detect(headlines_lower, "(jail)|(prison)|(inmate)|(incarcer)")
not_dupe <- !duplicated(headlines_lower)

relevant <- search_res[covid_related & jail_related & not_dupe,]

readr::write_csv(relevant, here::here("data", "explorer_subset02.csv"))
