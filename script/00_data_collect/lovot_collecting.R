# Sys.setlocale("LC_ALL", "Japanese")
library(rtweet)

# retrieve vaccine-related tweets
setwd('Documents/robot/')

lovot_tweets <- search_tweets(q = "'lovot' OR 'ラボット' OR 'らぼっと'",
                             n=100000, retryonratelimit = TRUE, tweet_mode = 'extended',
                             since='2020-08-14', until='2020-08-19')

  # save file
library(dplyr)
lovot_tweets <- dplyr::distinct(lovot_tweets, status_id, .keep_all = TRUE)
save_as_csv(lovot_tweets, "raw_data/lovot/raw_lovot_20Aug14-18.csv")

  # re-read data for converting it into tibble format
rm(lovot_tweets)
lovot_tweets_2 <- read_twitter_csv('raw_data/lovot/raw_lovot_20Aug01-04.csv')


# deal with unicode....
library(stringi)
lovot_tweets$text <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot_tweets$text))
lovot_tweets$name <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot_tweets$name))
lovot_tweets$hashtags <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot_tweets$hashtags))
lovot_tweets$location <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot_tweets$location))
lovot_tweets$description <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot_tweets$description))
lovot_tweets$retweet_text <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot_tweets$retweet_text))
lovot_tweets$retweet_name <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot_tweets$retweet_name))
lovot_tweets$retweet_location <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot_tweets$retweet_location))
lovot_tweets$retweet_description <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot_tweets$retweet_description))
lovot_tweets$quoted_text <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot_tweets$quoted_text))
lovot_tweets$quoted_name <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot_tweets$quoted_name))
lovot_tweets$quoted_location <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot_tweets$quoted_location))
lovot_tweets$quoted_description <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot_tweets$quoted_description))

  # check unicode characters
head(lovot_tweets$text)


# save unicode-ready file
library(readr)
write_excel_csv(lovot_tweets, 'raw_data/lovot/raw_lovot_20Jun27-30.csv')


# merge datesets
library(dplyr)

rm(lovot_tweets)
lovot_all <- read_csv('raw_data/lovot/lovot_all_Jun16.csv')
lovot_new <- read_csv('raw_data/lovot/raw_lovot_20Jun17-22.csv')
lovot_all <- bind_rows(lovot_all, lovot_new)
write_excel_csv(lovot_all, 'raw_data/lovot/lovot_all_Jun22.csv')
