# Sys.setlocale("LC_ALL", "Japanese")
library(rtweet)

# retrieve vaccine-related tweets
setwd('~/robot/')


aibo_tweets <- search_tweets(q = 'aibo OR アイボ',
                              n=100000, retryonratelimit = TRUE, tweet_mode = 'extended',
                              since='2020-08-19', until='2020-08-25')

## remove duplicated tweets by status_id
aibo_tweets <- dplyr::distinct(aibo_tweets, status_id, .keep_all = TRUE)

## save file 
save_as_csv(aibo_tweets, "raw_data/aibo/raw_aibo_20Aug14-18.csv")

# re-read data for converting it into tibble format
rm(aibo_tweets)
aibo_tweets <- read_twitter_csv('raw_data/aibo/raw_aibo_20Jun27-30.csv')


# deal with unicode....
library(stringi)
aibo_tweets$text <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", aibo_tweets$text))
aibo_tweets$name <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", aibo_tweets$name))
aibo_tweets$hashtags <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", aibo_tweets$hashtags))
aibo_tweets$location <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", aibo_tweets$location))
aibo_tweets$description <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", aibo_tweets$description))
aibo_tweets$retweet_text <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", aibo_tweets$retweet_text))
aibo_tweets$retweet_name <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", aibo_tweets$retweet_name))
aibo_tweets$retweet_location <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", aibo_tweets$retweet_location))
aibo_tweets$retweet_description <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", aibo_tweets$retweet_description))
aibo_tweets$quoted_text <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", aibo_tweets$quoted_text))
aibo_tweets$quoted_name <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", aibo_tweets$quoted_name))
aibo_tweets$quoted_location <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", aibo_tweets$quoted_location))
aibo_tweets$quoted_description <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", aibo_tweets$quoted_description))

# check unicode characters
head(aibo_tweets$text)


# save unicode-ready file
library(readr)
write_excel_csv(aibo_tweets, 'raw_data/aibo/raw_aibo_20Jun27-30.csv')


# merge datesets
library(dplyr)

aibo_all <- read_csv('raw_data/aibo/aibo_all_20Jun22.csv')
aibo_new <- read_csv('raw_data/aibo/raw_aibo_20Jun23-26.csv')

aibo_all <- bind_rows(aibo_all, aibo_new)
write_excel_csv(aibo_all, 'raw_data/aibo/aibo_all_20Jun22.csv')

