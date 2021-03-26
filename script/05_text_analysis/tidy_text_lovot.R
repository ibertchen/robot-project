library(here)
library(dplyr)
library(stringr)
library(RMeCab)
library(tidytext)
library(quanteda)
library(readr)

# read Twitter data
md <- rtweet::read_twitter_csv("data/clean_data/202005-08/all_clean_20201104_v2.csv")


# 1) fiter out retweet & non-aibo tweets
# 2) remove items from text: URL, hashtag, mention, un-decoded emoji and line break (\n)
# 3) filter out empty tweet  
lovot <- md %>% 
  filter(is_retweet==FALSE & is_lovot==TRUE) %>% 
  select(status_id, text) %>% 
  mutate(clean_text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp", " ") %>%  #remove URL
           str_replace_all("#[[:alnum:]_]+", " ") %>%  #remove hashtag
           str_replace_all("@[[:alnum:]_]+", " ") %>%  #remove mention
           str_replace_all("[<](.*?)[>]", " ") %>% #remove unrecognized emoji
           str_squish()) %>% #remove extra space
  filter(nchar(clean_text) > 0) %>%  #filter out empty text
  select(-text)


# tokenize (Morphological analysis with Mecab)
lovot$text <-  ""
lovot$text <- lovot$clean_text %>% 
  purrr::map(function(x) unlist(RMeCabC(x, dic = "mecab-dic/mecab-user-dict-seed.20200910.csv.dic")))

lovot <- select(lovot, -clean_text)


lovot_toks <- lovot %>% 
  tidyr::unnest(text) %>% 
  rename(word = text)

# remove japanese stopwords
jstopwords <- stopwords::stopwords("ja", source = "marimo")

lovot_toks <- lovot_toks %>% 
  mutate(stopw = word %in% jstopwords) %>% 
  filter(stopw == FALSE) %>% 
  select(-stopw)

# remove words used fewer than 5 times
rareword <- lovot_toks %>% 
  group_by(word) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  filter( n < 5) %>% 
  ungroup()

lovot_toks <- anti_join(lovot_toks, rareword, by = "word")

# remove specific types of phrases: 
p_types <- c("助動詞", "助詞", "副詞", "その他", "連体詞", "接続詞", "接頭詞")

lovot_toks <- lovot_toks %>% 
  mutate(ptypes = str_detect(names(word), pattern = paste(p_types, collapse = "|"))) %>% 
  filter(ptypes == FALSE) %>% 
  select(-ptypes)


# check nchar, kanji, katakana
# remove punctuation and emoji
lovot_toks <- lovot_toks %>% 
  mutate(word_len = nchar(word),
         is_kanji = str_detect(word, "\\p{han}"),
         is_kana = str_detect(word, "\\p{katakana}"),
         is_eng_num = str_detect(word, "[0-9a-zA-Z]"),
         is_punct = str_detect(word, "\\p{punct}"),
         is_emoji = str_detect(word, "\\p{emoji}")) %>% 
  filter(!is_punct == TRUE) %>% 
  filter(!is_emoji == TRUE) %>% 
  filter(!(nchar(word) < 2 & is_kanji == FALSE))  # remove single-character word


# remove word_length < 3 and hiragana-only word
gana <- lovot_toks %>% 
  filter(word_len <3 & is_kanji == F & is_kana == F & is_emoji == F & is_eng_num == F) %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(desc(n))

lovot_toks <- anti_join(lovot_toks, gana, by = "word")

# remove lovot-words, single
lovotword <- c("lovot", "ラボット", "らぼっと")

lovot_toks <- lovot_toks %>% 
  mutate( lovots = str_detect(str_to_lower(word), paste(lovotword, collapse = "|"))) %>% 
  filter(lovots == FALSE) %>% 
  select(-lovots)

# remove unnecessary columns
lovot_toks <- lovot_toks %>% 
  select(status_id, word)


# remove single character but keep "寝"
lovot_toks <- lovot_toks %>% 
  mutate(sleep = str_detect(word, "寝")) %>% 
  filter(!(nchar(word) <2 & sleep == FALSE))


# word frequency
lovot_freq <- lovot_toks %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(desc(n))

library(ggplot2)
p_lovot <- ggplot(lovot_freq[1:50,], aes(x=reorder(word, n), y=n)) + 
  geom_point() +
  coord_flip() +
  labs(x= "", y = "Percent.", title = "Lovot")

# co-occurence
library(widyr)

lovot_word_pair <- lovot_toks %>% 
  pairwise_count(word, status_id, sort=TRUE)

lovot_word_cor <- lovot_toks %>% 
  group_by(word) %>% 
  # filter(n() >= 2) %>% 
  pairwise_cor(word, status_id, sort = TRUE) %>% 
  filter(correlation < 0.999) %>%   #if cor=1 means one of the word is used only once
  filter(correlation > 0) # I don't care exclustion of words

hosip_word <- c("入院", "退院", "病院", "病気")
sleepword <- "寝"
birthword <- "生まれ"

lovot_word_cor %>% 
  filter(str_detect(item1, paste(birthword, collapse = "|"))) %>% 
  group_by(item1) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(item2 = reorder(item2, correlation)) %>% 
  ggplot(aes(reorder(item2, correlation), correlation)) +
  geom_col() +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  labs(x = "Phrase", y = "Correlation")

set.seed(2345)

library(igraph)
library(ggraph)

lovot_word_cor %>% 
  filter(str_detect(item1, paste(birthword, collapse = "|"))) %>% 
  filter(correlation > .065) %>%
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = F) +
  geom_node_point(color = "#00cc99", size = 5) +
  geom_node_text(aes(label = name), repel= T) +
  theme_void()

