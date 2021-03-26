md <- rtweet::read_twitter_csv("data/clean_data/after_Nov/all_clean_20201101-04.csv")


library(dplyr)
library(stringr)

md2 <- md %>% 
  mutate(is_aibo = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp", " ") %>%  #remove URL
           str_replace_all("@[[:alnum:]_]+", " ") %>%  #remove mention
           str_trim() %>% 
           str_to_lower() %>% 
           str_detect("aibo|アイボ|あいぼ")) %>% #check if aibo keywords in text
  mutate(is_lovot = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp", " ") %>%  #remove URL
           str_trim() %>% 
           str_to_lower() %>% 
           str_detect("lovot|ラボット|らぼっと")) #check if lovot keywords in text

clean_tweets_robot %>% 
  filter(is_aibo == TRUE | is_lovot==TRUE) %>% #at least aibo or lovot is mentioned
  select(-is_robot) %>% 
  rtweet::write_as_csv(here::here("data/clean_data/after_Nov/all_clean_20201105-08.csv"))

md2 %>% 
  filter(is_aibo==TRUE | is_lovot == TRUE) %>% 
  rtweet::write_as_csv(here::here("data/clean_data/after_Nov/all_clean_20201101-04_v2.csv"))


md2 <- md2 %>% 
  filter(is_aibo==TRUE | is_lovot == TRUE)


aibo_list <- c("aibo", "アイボ", "あいぼ")


img_list <- rtweet::read_twitter_csv("img_url_list/after_Nov/robot_img_20201101-04.csv")

str(img_list)

good_id <- md2 %>% 
  filter(media_type != "NA") %>% 
  select(status_id)

img_list2 <- inner_join(img_list, good_id, by = "status_id")

rtweet::write_as_csv(img_list2, "img_url_list/after_Nov/robot_img_20201101-04_v2.csv")

del_list <- anti_join(img_list, good_id, by = "status_id")

setwd("imgs/")

for (i in 1:nrow(del_list)) {
  img_name <- paste0(del_list$img_id[i], ".jpg")
  print(img_name)
}
