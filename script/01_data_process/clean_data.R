## filtering tweets without keywords in text, retweet_text, or quoted_text

# Sys.setlocale("LC_ALL", "Japanese")
# library(readr)

## AIBO
## read data
md <- read.csv('raw_data/aibo/aibo_202007.csv', encoding = 'UTF-8', stringsAsFactors = F)

# md <- md %>% 
#   select(-X.U.FEFF.user_id)


## remove tweets which don't have "aibo" in text
library(dplyr)
library(stringr)

md_clean <- md %>% 
  distinct(status_id, .keep_all=TRUE) %>%  # remove duplicated tweets (it just happens...)
  filter(lang=="ja" | lang=='en') %>% 
  mutate(text2 = str_replace_all(text, "@\\S*", ""), 
         retweet_text2 = str_replace_all(retweet_text, "@\\S*", ""),
         quoted_text2 = str_replace_all(quoted_text, "@\\S*", "")) %>% 
  filter(str_detect(text2, "(aibo)|(アイボ)") | 
           str_detect(retweet_text2, "(aibo)|(アイボ)") | 
           str_detect(quoted_text2, "(aibo)|(アイボ)")) %>% 
  select(-text2, -retweet_text2, -quoted_text2) %>% 
  arrange(created_at)


# unicode... (if needed)
library(stringi)
md_clean$text <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$text))
md_clean$name <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$name))
md_clean$location <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$location))
md_clean$description <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$description))
md_clean$retweet_text <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$retweet_text))
md_clean$retweet_name <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$retweet_name))
md_clean$retweet_location <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$retweet_location))
md_clean$retweet_description <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$retweet_description))
md_clean$quoted_text <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$quoted_text))
md_clean$quoted_name <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$quoted_name))
md_clean$quoted_location <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$quoted_location))
md_clean$quoted_description <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$quoted_description))


## save clean data
library(readr)
write_csv(md_clean, 'clean_data/aibo_clean_202007_v2.csv')


##-----------------------
# LOVOT
# read data
md <- read.csv('raw_data/lovot/lovot_202007.csv', encoding = 'UTF-8', stringsAsFactors = F)


# remove tweets which don't have "aibo" in text
library(dplyr)
library(stringr)

md_clean <- md %>% 
  distinct(status_id, .keep_all=TRUE) %>% # remove duplicated tweets (it just happens...)
  filter(lang=="ja" | lang=='en') %>% 
  mutate(text2 = str_replace_all(text, "@\\S*", ""), 
         retweet_text2 = str_replace_all(retweet_text, "@\\S*", ""),
         quoted_text2 = str_replace_all(quoted_text, "@\\S*", "")) %>% 
  filter(str_detect(text2, "(lovot)|(ラボット)|(らぼっと)") | 
           str_detect(retweet_text2, "(lovot)|(ラボット)|(らぼっと)") | 
           str_detect(quoted_text2, "(lovot)|(ラボット)|(らぼっと)")) %>% 
  select(-text2, -retweet_text2, -quoted_text2) %>% 
  arrange(created_at)


# unicode
md_clean$text <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$text))
md_clean$name <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$name))
md_clean$location <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$location))
md_clean$description <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$description))
md_clean$retweet_text <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$retweet_text))
md_clean$retweet_name <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$retweet_name))
md_clean$retweet_location <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$retweet_location))
md_clean$retweet_description <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$retweet_description))
md_clean$quoted_text <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$quoted_text))
md_clean$quoted_name <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$quoted_name))
md_clean$quoted_location <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$quoted_location))
md_clean$quoted_description <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", md_clean$quoted_description))


## save clean data
library(readr)
write_csv(md_clean, 'clean_data/lovot_clean_202007_v2.csv')
