# 2020-11-03

# load packages
library(dplyr)
library(readr)
library(stringr)
library(here)


# keywords
word_list1 <- c("http\\S*", "<.[^ぁ-んァ-ン一-龯]*>")  #regex for URLs and unrecognized emoji
word_list2 <- c("aibo", "lovot", "アイボ", "ラボット", "あいぼ", "らぼっと")


# import data
lovot_tweets <- read_csv(here("data/raw_data/lovot_raw_all_20201103.csv"))

aibo_tweets <- read_csv(here("data/raw_data/aibo_raw_all_20201103.csv"))

# check if robots are mentioned in tweets
lovot_clean <- lovot_tweets %>% 
  mutate(is_robot = str_remove_all(text, paste(word_list1, collapse="|")) %>% 
           str_to_lower() %>% 
           str_detect(pattern = paste(word_list2, collapse="|"))
         ) %>% 
  filter(is_robot == TRUE & lang == "ja")


aibo_clean <- aibo_tweets %>% 
  mutate(is_robot = str_remove_all(text, paste(word_list1, collapse="|")) %>% 
           str_to_lower() %>% 
           str_detect(pattern = paste(word_list2, collapse="|"))
         ) %>% 
  filter(is_robot == TRUE & lang == "ja")


# save file
lovot_clean %>% 
  select(-is_robot) %>% 
  write_csv("data/clean_data/lovot_clean_20201103.csv")

aibo_clean %>% 
  select(-is_robot) %>% 
  write_csv("data/clean_data/aibo_clean_20201103.csv")
