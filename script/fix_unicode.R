library(here)
library(rtweet)
library(dplyr)
library(stringr)

md <- read_twitter_csv(here("data/clean_data/202005-08/all_clean_20201104_v1.csv"))

fixuni <- function(x) {
  str_remove_all(x, ">") %>% 
    str_replace_all("<U\\+", "\\\\U") %>% 
    stringi::stri_unescape_unicode()
}

md$text <- purrr::map_chr(md$text, fixuni)

md$hashtags <- purrr::map(md$hashtags, fixuni)

write_as_csv(md, here("data/clean_data/202005-08/all_clean_20201104_v2.csv"))

