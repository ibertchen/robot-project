# 2020-11-03

# load packages
library(readr)
library(dplyr)
library(jpeg)
library(here)
library(lubridate)


# lovot
# import data
lovot <- read_csv(here("data/clean_data/lovot_clean_20201103.csv")) %>% 
  mutate(post_date = date(created_at))


# generate image-url list
## filter out tweets which are retweets and having no media file
lovot_img <- lovot %>% 
  filter(is_retweet == "FALSE" & media_type != "NA") %>% 
  select(status_id, screen_name, name, text, post_date, ext_media_url) %>% 
  mutate(img_url = stringr::str_split(ext_media_url, " ")) %>% 
  select(-ext_media_url) %>% 
  tidyr::unnest(img_url) %>% 
  mutate(img_id = paste0("lovot_", row_number()))

## save image-url list
lovot_img %>% write_csv(here("img_url_list/lovot_img_20201103.csv"))


# download image
kk <- lovot_img %>% 
  filter(post_date > "2020-05-31")

for (i in 1:nrow(kk)) {
  tryCatch({
    img_name <- paste0(kk$img_id[i], ".jpg")
    z <- tempfile()
    download.file(kk$img_url[i], z, mode="wb")
    img <- readJPEG(z)
    writeJPEG(img, paste0(here("images/lovot/", img_name)))
    }, error=function(e){})
  if (i%%100 == 0) {
    print(paste0("current #: ", i)); flush.console()
    Sys.sleep(13)
    cat("\014")
  }
}


# ----------------
# aibo
# import data
aibo <- read_csv(here("data/clean_data/aibo_clean_20201103.csv")) %>% 
  mutate(post_date = date(created_at))

# generate image-url list
## filter out tweets which are retweets and having no media file
aibo_img <- aibo %>% 
  filter(is_retweet == "FALSE" & media_type != "NA") %>% 
  select(status_id, screen_name, name, text, post_date, ext_media_url) %>% 
  mutate(img_url = stringr::str_split(ext_media_url, " ")) %>% 
  select(-ext_media_url) %>% 
  tidyr::unnest(img_url) %>% 
  mutate(img_id = paste0("aibo_", row_number()))

## save image-url list
aibo_img %>% write_csv(here("img_url_list/aibo_img_20201103.csv"))


# download image
kk <- aibo_img %>% 
  filter(post_date > "2020-7-31")

for (i in 1:nrow(kk)) {
  tryCatch({
    img_name <- paste0(kk$img_id[i], ".jpg")
    z <- tempfile()
    download.file(kk$img_url[i], z, mode="wb")
    img <- readJPEG(z)
    writeJPEG(img, paste0(here("images/aibo/", img_name)))
  }, error=function(e){})
  if (i%%120 == 0) {
    print(paste0("current #: ", i)); flush.console()
    Sys.sleep(15)
    cat("\014")
  }
}
