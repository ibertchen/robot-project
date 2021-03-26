## Last modified: 20201106
# required packages: here, rtweet, dplyr, stringr, jpeg


# declare working directory location
here::i_am("script/all_in_one.R")

## ---- Collect Data ---- ##
# prefix-
newfile <- "20210321-24"

## search and retrieve tweets
# remember to modify date (since & until)
library(rtweet)
robot_tweets <- search_tweets(q= "aibo OR lovot OR アイボ OR ラボット OR あいぼ OR らぼっと",
                              n=100000, retryonratelimit = TRUE,
                              tweet_mode = "extended", lang = "ja",
                              since = "2021-03-21", until = "2021-03-25")

## sort data by time
library(dplyr)
robot_tweets <- robot_tweets %>% distinct(status_id, .keep_all = TRUE)
robot_tweets <- robot_tweets %>% arrange(status_id)

head(robot_tweets$created_at, 10)
tail(robot_tweets$created_at, 10)
ts_plot(robot_tweets, "hours") + ggplot2::theme_minimal()


## save raw data to file
library(here)
write_as_csv(robot_tweets, here("data", "raw_data", paste0("raw_", newfile, ".csv")))



## ---- Clean Data ---- ##
library(stringr)

## import data
# md <- rtweet::read_twitter_csv(here::here("data/raw_data/after_Nov/raw_20201113-16.csv"))
md <- robot_tweets

## filter out tweets that have no keywords in text content
robot_words <- c("aibo", "lovot", "アイボ", "ラボット", "あいぼ", "らぼっと")
remove_words <- c("http\\S*", "[<](.*?)[>]")

clean_tweets <- md %>% 
  mutate(is_robot = str_remove_all(text, paste(remove_words, collapse = "|")) %>% 
           str_trim() %>% 
           str_to_lower() %>% 
           str_detect(paste(robot_words, collapse = "|"))) # Having keywords: is_robot == TRUE


## check if aibo or lovot are mentioned
# a strict rule for aibo: aibo must appears in text or hashtag (excluding username)
clean_tweets_robot <- clean_tweets %>% 
  filter(is_robot==TRUE) %>% 
  mutate(is_aibo = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp", " ") %>%  #remove URL
           str_replace_all("@[[:alnum:]_]+", " ") %>%  #remove mention
           str_trim() %>% 
           str_to_lower() %>% 
           str_detect("aibo|アイボ|あいぼ")) %>% #check if aibo keywords in text
  mutate(is_lovot = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp", " ") %>%  #remove URL
           str_trim() %>% 
           str_to_lower() %>% 
           str_detect("lovot|ラボット|らぼっと")) #check if lovot keywords in text
  


## save clean data to file
clean_tweets_robot %>% 
  filter(is_aibo == TRUE | is_lovot==TRUE) %>% #at least aibo or lovot is mentioned
  select(-is_robot) %>% 
  write_as_csv(here("data", "clean_data", paste0("all_clean_", newfile, ".csv")))




## ---- Get Images ---- ##

## import data
md <- read_twitter_csv(here("data", "clean_data", paste0("all_clean_", newfile, ".csv")))


## create img_url table
aibo_list <- c("aibo", "アイボ", "あいぼ")

# strict rules on aibo-related tweets: keywords must appear in text, excluding mentions
robot_img <- md %>% 
  filter(is_retweet == FALSE & media_type != "NA") %>% 
  mutate(post_date = lubridate::date(created_at)) %>% 
  select(status_id, screen_name, name, text, post_date, is_aibo, is_lovot, ext_media_url) %>% 
  mutate(img_url = stringr::str_split(ext_media_url, " ")) %>% 
  select(-ext_media_url) %>% 
  tidyr::unnest(img_url) %>% 
  mutate(img_id = paste0(post_date, "_", row_number()))

write_as_csv(robot_img, here("img_url_list", paste0("robot_img_", newfile, ".csv")))  #save output file


## download image
library(jpeg)

for (i in 1:nrow(robot_img)) {
  tryCatch({
    img_name <- paste0(robot_img$img_id[i], ".jpg")
    z <- tempfile()
    download.file(robot_img$img_url[i], z, mode="wb")
    img <- readJPEG(z)
    writeJPEG(img, paste0(here("image", "temp", img_name)))
  }, error=function(e){})
  if (i%%120 == 0) {
    print(paste0("current #: ", i)); flush.console()
    Sys.sleep(15)
    cat("\014")
  }
}

