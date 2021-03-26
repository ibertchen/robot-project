## Twitter hashtags analysis

## load packages
library(here)
library(pipeR)
library(dplyr)
library(lubridate)
library(stringr)

## load data (use clean data)
clean_tweet <- read.csv(here("robot/report", "clean_all.csv"), encoding = "UTF-8", stringsAsFactors = F) %>>%
  distinct(status_id, .keep_all = TRUE) %>>%
  select(status_id, user_id, created_at, screen_name, is_retweet, hashtags, ext_media_url, robot)


## filter out retweet
clean_ori_tweet <- clean_tweet %>>%
  filter(is_retweet==FALSE)

## filter out tweets without hashtags
hashtag_tweet <- clean_ori_tweet %>>%
  filter(hashtags!="NA")

## split hashtags
hashtag_tweet$hashtags <- str_split(hashtag_tweet$hashtags, " ")

## unnest hashtags
hashtag_tweet <- hashtag_tweet %>>%
  tidyr::unnest(hashtags)
  
hashtag_tweet$hashtags <- str_to_lower(hashtag_tweet$hashtags) # convert to lowercase letters

## summarize hashtags
## aibo
aibo_hashtag <- hashtag_tweet %>>%
  filter(robot=="aibo") %>>%
  group_by(hashtags) %>>%
  summarize(hashtag_n = n()) %>>%
  arrange(desc(hashtag_n)) %>>%
  ungroup()

lovot_hashtag <- hashtag_tweet %>>%
  filter(robot=="lovot") %>>%
  group_by(hashtags) %>>%
  summarize(hashtag_n = n()) %>>%
  arrange(desc(hashtag_n)) %>>%
  ungroup()


## summarize hashtags
ht_data %>% 
  select(hashtags) %>% 
  group_by(hashtags) %>% 
  summarize(count = n()) %>%
  arrange(desc(count)) %>% 
  ungroup()
