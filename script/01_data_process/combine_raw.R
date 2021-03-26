setwd('robot')

dir()

library(rtweet)
library(dplyr)

# Sys.setlocale("LC_ALL", "Japanese")
# options(scipen=999)

md1 <- read_twitter_csv('raw_data/aibo/raw_aibo_20May22-29.csv')
md2 <- read_twitter_csv('raw_data/aibo/raw_aibo_20May30-Jun03.csv')
md3 <- read_twitter_csv('raw_data/aibo/raw_aibo_20Jun04-08.csv')
md4 <- read_twitter_csv('raw_data/aibo/raw_aibo_20Jun09-12.csv')
md5 <- read_twitter_csv('raw_data/aibo/raw_aibo_20Jun13-16.csv')
md6 <- read_twitter_csv('raw_data/aibo/raw_aibo_20Jun17-22.csv')
md7 <- read_twitter_csv('raw_data/aibo/raw_aibo_20Jun23-26.csv')
md8 <- read_twitter_csv('raw_data/aibo/raw_aibo_20Jun27-30.csv')

# escape unicode characters (if needed)
library(stringi)
md1$text <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md1$text))
md1$name <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md1$name))
md1$hashtags <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md1$hashtags))
md1$location <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md1$location))
md1$description <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md1$description))
md1$retweet_text <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md1$retweet_text))
md1$retweet_name <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md1$retweet_name))
md1$retweet_location <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md1$retweet_location))
md1$retweet_description <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md1$retweet_description))
md1$quoted_text <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md1$quoted_text))
md1$quoted_name <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md1$quoted_name))
md1$quoted_location <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md1$quoted_location))
md1$quoted_description <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md1$quoted_description))

## fix column name problem (X.U.FEFF.user_id)
md5 <- rename(md5, user_id=X.U.FEFF.user_id)
md6 <- rename(md6, user_id=X.U.FEFF.user_id)
md7 <- rename(md7, user_id=X.U.FEFF.user_id)
md8 <- rename(md8, user_id=X.U.FEFF.user_id)


## merge datasets
aibo_1 <- bind_rows(md1, md2, md3, md4)
aibo_1 <- distinct(aibo_1, .keep_all = TRUE)

aibo_2 <- bind_rows(md5, md6, md7, md8)
aibo_2 <- distinct(aibo_2, .keep_all = T)


## fix variable type issue
kk <- cbind(map(aibo_1, class), map(aibo_2, class))

for (i in 1:nrow(kk)) {
  if (kk[i,1][[1]] != kk[i,2][[1]]) {
    print(paste(i, names(kk[i,1]), kk[i,1][[1]], kk[i,2][[1]]))
  }
}
  
aibo_2[,15:16] <- map(aibo_2[,15:16], as.logical)
aibo_2$ext_media_type <- as.logical(aibo_2$ext_media_type)
aibo_2[,37:38] <- map(aibo_2[,37:38], as.integer)
aibo_2[,42:44] <- map(aibo_2[,42:44], as.integer)
aibo_2$quoted_verified <- as.logical(aibo_2$quoted_verified)
aibo_2[,52:53] <- map(aibo_2[,52:53], as.integer)
aibo_2[,57:59] <- map(aibo_2[,57:59], as.integer)
aibo_2$retweet_verified <- as.logical(aibo_2$retweet_verified)
aibo_2$account_lang <- as.logical(aibo_2$account_lang)


## combine files...
aibo <- bind_rows(aibo_1, aibo_2)


## save merged file
## use readr::write_csv to avoid unicode issue
library(readr)
write_csv(aibo, 'raw_data/aibo/aibo_202006_new.csv')




## ---- LOVOT ---- ##
md1 <- read_twitter_csv('raw_data/lovot/raw_lovot_20Jul01-04.csv')
md2 <- read_twitter_csv('raw_data/lovot/raw_lovot_20Jul05-08.csv')
md3 <- read_twitter_csv('raw_data/lovot/raw_lovot_20Jul09-12.csv')
md4 <- read_twitter_csv('raw_data/lovot/raw_lovot_20Jul13-17.csv')
md5 <- read_twitter_csv('raw_data/lovot/raw_lovot_20Jul18-23.csv')
md6 <- read_twitter_csv('raw_data/lovot/raw_lovot_20Jul24-27.csv')
md7 <- read_twitter_csv('raw_data/lovot/raw_lovot_20Jul28-31.csv')
# md8 <- read_twitter_csv('raw_data/lovot/raw_lovot_20Jun27-30.csv')

lovot_1 <- bind_rows(md1, md2, md3, md4)
lovot_2 <- bind_rows(md5, md6, md7)

# lovot_2 <- rename(lovot_2, user_id=X.U.FEFF.user_id)

## fix variable type issue
library(purrr)

kk <- cbind(map(lovot_1, class), map(lovot_2, class))

for (i in 1:nrow(kk)) {
  if (kk[i,1][[1]] != kk[i,2][[1]]) {
    print(paste(i, names(kk[i,1]), kk[i,1][[1]], kk[i,2][[1]]))
  }
}

lovot_2[,15:16] <- map(lovot_2[,15:16], as.logical)
lovot_2$symbols <- as.logical(lovot_2$symbols)
lovot_2$ext_media_type <- as.logical(lovot_2$ext_media_type)
lovot_2[,37:38] <- map(lovot_2[,37:38], as.integer)
lovot_2[,42:44] <- map(lovot_2[,42:44], as.integer)
lovot_2$quoted_verified <- as.logical(lovot_2$quoted_verified)
lovot_2[,52:53] <- map(lovot_2[,52:53], as.integer)
lovot_2[,57:59] <- map(lovot_2[,57:59], as.integer)
lovot_2$retweet_verified <- as.logical(lovot_2$retweet_verified)
lovot_2$account_lang <- as.logical(lovot_2$account_lang)


## combine files...
lovot <- bind_rows(lovot_1, lovot_2)

# escape unicode characters (if needed)
library(stringi)
lovot$text <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot$text))
lovot$name <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot$name))
lovot$hashtags <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot$hashtags))
lovot$location <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot$location))
lovot$description <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot$description))
lovot$retweet_text <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot$retweet_text))
lovot$retweet_name <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot$retweet_name))
lovot$retweet_location <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot$retweet_location))
lovot$retweet_description <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot$retweet_description))
lovot$quoted_text <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot$quoted_text))
lovot$quoted_name <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot$quoted_name))
lovot$quoted_location <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot$quoted_location))
lovot$quoted_description <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", lovot$quoted_description))

lovot$text[6000:6005]


## save merged file
## use readr::write_csv to avoid unicode issue
library(readr)
lovot <- distinct(lovot)
write_csv(lovot, 'raw_data/lovot/lovot_202007_new.csv')
