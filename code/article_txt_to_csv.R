library(tidyverse)

dat <- read_file(here::here("data", "search01.txt"))

# there seemed to be some publication date mess-ups so try to fix
dat <- str_replace_all(dat, "Publicationdate\\:", "Publication date:")

# this is the header that splits up the different articles
# first entry can be removed (before Document 1)
split_dat <- str_split(dat, "(Document)\\s([0-9])+\\s(of)\\s([0-9])+")[[1]][-1]

# these strings split up the info within each entry
var_names <- c("Author:","Publication info:","Abstract:","Links:","Full text:",
               "Subject:","Location:","Company / organization:","Title:",
               "Publication title:","Publication year:","Publication date:",
               "Publisher:","Place of publication:","Country of publication:",
               "Publication subject:","Source type:","Language of publication:",
               "Document type:","ProQuest document ID:","Document URL:","Copyright:",
               "Last updated:","Database:")

# function to split by one of the strings above
# the text before the split belongs to the last variable type
# and the text after the split is passed on to look for the next split
split_for_next <- function(article, split_var) {
  
  split_1 <- str_split(article, coll(split_var))[[1]]
  
  if (length(split_1) > 1) { # if split_var doesn't exist in the text
    keep <- split_1[1]
    pass <- paste(split_1[-1], collapse = " ")
  } else {
    keep <- NA_character_
    pass <- split_1[1]
  }
  
  return(list(keep, pass))
}

# function to go through all the var_names doing the appropriate splits
split_article <- function(article, var_names) {
  
  keep <- vector(mode = "character", length = length(var_names) + 1) #initiate
  # the first part doesn't have a name -- everything before var_name[1]
  article <- split_for_next(article, var_names[1])
  # but for some of the articles var_name[1] doesn't exist!
  keep[1] <- ifelse(is.na(article[[1]]), NA_character_, str_squish(article[[1]]))

  for (i in seq_along(var_names)[-1]) {
    article <- split_for_next(article[[2]], var_names[i])
    keep[i] <- str_squish(article[[1]])
  }
  
  names(keep) <- c("headline", str_remove(var_names, "\\:"))
  
  # this is where the ones without the first variable get fixed
  if (is.na(keep[1])) {
    keep[1] <- keep[2]
    keep[2] <- NA_character_
  }
  
  return(keep)
}

# do it too all the articles and make into a dataframe, some light cleaning
articles_split <- split_dat %>%
  map(split_article, var_names) %>% 
  reduce(bind_rows) %>%
  janitor::clean_names() %>%
  mutate(date = str_sub(publication_date, 1, 12),
         date = str_remove(date, "\\D$"),
         date = parse_date(date, format = "%b %d, %Y")) %>%
  select(-database)

# save as csv
write_csv(articles_split, here::here("data", "search01.csv"))
# and as rds
write_rds(articles_split, here::here("data", "search01.rds"))
