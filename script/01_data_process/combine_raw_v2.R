# 2020-11-03

# load packages
library(rtweet)
library(dplyr)
library(stringi)
library(here)


# import data
## set file path
lovot_files <- list.files(path = here("data/raw_data/lovot/"), 
                          pattern = "*.csv", full.names = TRUE)

aibo_files <- list.files(path = here("data/raw_data/aibo/"), 
                         pattern = "*.csv", full.names = TRUE)


## read data
lovot_tweets <- plyr::ldply(lovot_files, read_twitter_csv) %>% 
  arrange(created_at) %>% 
  distinct(status_id, .keep_all = TRUE)
  

aibo_tweets <- plyr::ldply(aibo_files, read_twitter_csv) %>% 
  arrange(created_at) %>% 
  distinct(status_id, .keep_all = TRUE)


## (fix unicode display issue)
lovot_tweets$text <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot_tweets$text))

aibo_tweets$text <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", aibo_tweets$text))



# save files
lovot_tweets %>% 
  filter(text != "NA") %>% 
  readr::write_csv(here("data/raw_data/lovot_raw_all_20201103.csv"))

aibo_tweets %>% 
  filter(text != "NA") %>% 
  readr::write_csv(here("data/raw_data/aibo_raw_all_20201103.csv"))
