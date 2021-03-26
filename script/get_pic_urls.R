# Get pic URLs 
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

Sys.setlocale("LC_ALL", "Japanese")
options(scipen=999) # makes all numbers to appear as decimals
setwd('~/robot/')

# aibo
aibo <- read.csv('clean_data/aibo_clean_202007.csv', encoding = 'UTF-8', stringsAsFactors = F)
aibo$status_id <- as.character(aibo$status_id)
aibo$user_id <- as.character(aibo$user_id)
aibo$created_at <- ymd_hms(aibo$created_at)

aibo_pics <- aibo %>% 
  # filter((created_at < "2020-07-31") & (created_at > "2020-07-01")) %>%
  filter(is_retweet==F & ext_media_url != 'NA') %>% 
  select(status_id, user_id, screen_name, name, created_at, text, ext_media_url) %>% 
  mutate(urls=str_split(ext_media_url, " ")) %>% 
  select(-ext_media_url) %>% 
  unnest(urls) %>% 
  mutate(pic_id = row_number())

## modify pic_id
aibo_pics$pic_id <- paste0('20_07_', as.character(aibo_pics$pic_id))  #pic_id for images after 2020-06-13


## save aibo_pics (after 2020-06-13)
write_csv(aibo_pics, '~/robot/pic_table/aibo_20_07.csv') # DON'T OVERWRITE THIS FILE!!!!!


# download image
library(imager)
setwd('~/robot/photos/aibo/20_07/')

for (i in 1:nrow(aibo_pics)) {                   
  tryCatch({
    pname <- paste0(aibo_pics$pic_id[i], '.jpg')
    save.image(load.image(aibo_pics$urls[i]), pname)
    pic_n <- pic_n + 1
  }, error=function(e){})
  if (i%%100==0) {
    print(pic_n)
    Sys.sleep(15)}
}


# making table
library(knitr)
library(kableExtra)
aibo_pics %>% 
  kable(format = 'html') %>% 
  kable_styling(bootstrap_options = c('hover', 'condensed'))

####--------
## lovot
lovot <- read.csv('clean_data/lovot_clean_202007.csv', encoding='UTF-8', stringsAsFactors = F)
lovot$status_id <- as.character(lovot$status_id)
lovot$user_id <- as.character(lovot$user_id)
lovot$created_at <- ymd_hms(lovot$created_at)

lovot_pics <- lovot %>% 
  # filter(created_at > '2020-06-01') %>%
  filter(is_retweet==F & ext_media_url != 'NA') %>% 
  select(status_id, screen_name, name, created_at, text, ext_media_url) %>% 
  mutate(urls=str_split(ext_media_url, " ")) %>% 
  select(-ext_media_url) %>% 
  unnest(urls) %>% 
  mutate(pic_id = row_number())

## modify pic_id
lovot_pics$pic_id <- paste0('20_07_', as.character(lovot_pics$pic_id))  #pic_id for images after 2020-06-13

## save aibo_pics (after 2020-06-13)
write_csv(lovot_pics, '~/robot/pic_table/lovot_20_07.csv') # DON'T OVERWRITE THIS FILE!!!!!


# download image
library(imager)
setwd('~/robot/photos/lovot/20_07/')

for (i in 1:nrow(lovot_pics)) {                   # remember to change nrow number 
  tryCatch({
    pname <- paste0(lovot_pics$pic_id[i], '.jpg')
    save.image(load.image(lovot_pics$urls[i]), pname)
    pic_n <- pic_n + 1
  }, error=function(e){})
  if (i%%100==0) {
    print(pic_n)
    Sys.sleep(15)}
}
