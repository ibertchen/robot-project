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
aibo <- md %>% 
  filter(is_retweet==FALSE & is_aibo==TRUE) %>% 
  select(status_id, text) %>% 
  mutate(clean_text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp", " ") %>%  #remove URL
           str_replace_all("#[[:alnum:]_]+", " ") %>%  #remove hashtag
           str_replace_all("@[[:alnum:]_]+", " ") %>%  #remove mention
           str_replace_all("[<](.*?)[>]", " ") %>% #remove unrecognized emoji
           str_squish()) %>% #remove extra space
  filter(nchar(clean_text) > 0) %>%  #filter out empty text
  select(-text)


# tokenize (Morphological analysis with Mecab)
aibo$text <-  ""
aibo$text <- aibo$clean_text %>% 
  purrr::map(function(x) unlist(RMeCabC(x, dic = "mecab-dic/mecab-user-dict-seed.20200910.csv.dic")))

aibo <- select(aibo, -clean_text)


## bigram
# aibo$text <- aibo$text %>% purrr::map_chr(function(x) paste(unlist(x), collapse = " "))
#   # str_remove_all(pattern = "\\p{punct}") %>%
#   # str_remove_all(pattern = "\\p{emoji}")
# 
# token_list <- aibo %>% 
#   unnest_tokens(word, text) %>% 
#   group_by(word) %>% 
#   summarise(n = n()) %>% 
#   arrange(desc(n)) %>% 
#   rename(count = n)
# 
# aibo_bigrams <- aibo %>% 
#   unnest_tokens(bigram, text, token = "ngrams", n = 2)
# 
# 
# library(tidyr)
# aibo_bigrams_sep <- aibo_bigrams %>% 
#   separate(bigram, c("word1", "word2"), sep = " ")
# 
# ja_stopwords <- tibble(word = stopwords::stopwords("ja", "marimo"))
# 
# aibo_bigrams_filtered <- aibo_bigrams_sep %>% 
#   mutate(puncts1 = str_detect(word1, "\\p{punct}"), 
#          puncts2 = str_detect(word2, "\\p{punct}")) %>% 
#   filter((puncts1 == FALSE & puncts2 == FALSE)) %>% 
#   select(-puncts1, -puncts2) %>% 
#   mutate(emoji1 = str_detect(word1, "\\p{emoji}"),
#          emoji2 = str_detect(word2, "\\p{emoji}")) %>% 
#   filter((emoji1 == FALSE & emoji2 == FALSE)) %>% 
#   select(-emoji1, -emoji2) %>% 
#   mutate(stop1 = word1 %in% ja_stopwords,
#          stop2 = word2 %in% ja_stopwords) %>% 
#   filter(stop1 == FALSE | stop2 == FALSE) %>% 
#   select(-stop1, -stop2) %>% 
#   mutate(gana1 = str_detect(word1, "[ぁ-ん]+"),
#          gana2 = str_detect(word2, "[ぁ-ん]+")) %>% 
#   filter(!(gana1 == TRUE & gana2 == TRUE & nchar(word1) < 2 & nchar(word2) < 2)) %>% 
#   select(-gana1, -gana2)
#   # unite(bigram, word1, word2, sep = " ")
# 
# aibo_bigrams_count <- aibo_bigrams_filtered %>% 
#   count(word1, word2, sort = TRUE)
# 
# aibo_bigrams_tfidf <- aibo_bigrams_filtered %>% 
#   count(status_id, bigram) %>% 
#   bind_tf_idf(bigram, status_id, n) %>% 
#   arrange(desc(tf_idf)) %>% 
#   filter(n > 1)
# 
# aibo_bigrams_tokens <- aibo_bigrams_tfidf %>% 
#   unnest_tokens(word, bigram) %>% 
#   group_by(word) %>% 
#   summarize(n = n()) %>% 
#   arrange(desc(n))
# 
# aibo_bigrams_tokens <- anti_join(aibo_bigrams_tokens, ja_stopwords, by = "word")
# 
# aibo_bigrams_tokens <- left_join(aibo_bigrams_tokens, token_list, by = "word") %>% 
#   arrange(desc(count))
# 
# aibo_bigrams_tokens <- aibo_bigrams_tokens %>% 
#   mutate(aibos = str_detect(str_to_lower(word), paste(c("aibo","アイボ","あいぼ"), collapse = "|"))) %>% 
#   filter(aibos == FALSE) %>% 
#   select(-aibos)
# 
# bad_word <- bind_rows(read_csv("副詞.csv"), read_csv("接続詞.csv"), read_csv("連体詞.csv"))
# 
# aibo_bigrams_tokens <- anti_join(aibo_bigrams_tokens, bad_word, by = "word")
# 
# aibo_bigrams_tokens <-  aibo_bigrams_tokens %>% 
#   filter(nchar(word)>1) %>% 
#   mutate(gana = (str_detect(word, "[ぁ-ん]+") & nchar(word)<3)) %>% 
#   filter(gana == FALSE) %>% 
#   select(-gana)
# 
# library(ggplot2)
# 
# aibo_bigrams_tokens[1:50,] %>% 
#   ggplot(aes(x=reorder(word, count), y=count)) +
#   geom_point() +
#   coord_flip()
# 
# aibo_bigrams_tokens %>% write_csv(here::here("tasks/text_analysis/aibo_term_list.csv"))


##

aibo_toks <- aibo %>% 
  tidyr::unnest(text) %>% 
  rename(word = text)

# remove japanese stopwords
jstopwords <- stopwords::stopwords("ja", source = "marimo")

aibo_toks <- aibo_toks %>% 
  mutate(stopw = word %in% jstopwords) %>% 
  filter(stopw == FALSE) %>% 
  select(-stopw)


# remove words used fewer than 5 times
rareword <- aibo_toks %>% 
  group_by(word) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  filter( n < 5) %>% 
  ungroup()

aibo_toks <- anti_join(aibo_toks, rareword, by = "word")

# remove specific types of phrases: 
p_types <- c("助動詞", "助詞", "副詞", "その他", "連体詞", "接続詞", "接頭詞")

aibo_toks <- aibo_toks %>% 
  mutate(ptypes = str_detect(names(word), pattern = paste(p_types, collapse = "|"))) %>% 
  filter(ptypes == FALSE) %>% 
  select(-ptypes)


# check nchar, kanji, katakana
# remove punctuation and emoji
aibo_toks <- aibo_toks %>% 
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
gana <- aibo_toks %>% 
  filter(word_len <3 & is_kanji == F & is_kana == F & is_emoji == F & is_eng_num == F) %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(desc(n))

aibo_toks <- anti_join(aibo_toks, gana, by = "word")

# remove aibo-words, single
aiboword <- c("aibo", "あいぼ", "アイボ")

aibo_toks <- aibo_toks %>% 
  mutate( aibos = str_detect(str_to_lower(word), paste(aiboword, collapse = "|"))) %>% 
  filter(aibos == FALSE) %>% 
  select(-aibos)

# remove unnecessary columns
aibo_toks <- aibo_toks %>% 
  select(status_id, word)


# remove single character but keep "寝"
aibo_toks <- aibo_toks %>% 
  mutate(sleep = str_detect(word, "寝")) %>% 
  filter(!(nchar(word) <2 & sleep == FALSE))


# word frequency
aibo_freq <- aibo_toks %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(desc(n))

library(ggplot2)
p_aibo <- ggplot(aibo_freq[1:50,], aes(x=reorder(word, n), y=n)) + 
  geom_point() +
  coord_flip() +
  labs(x = "Phrase", y="Frequency", title = "Aibo")

# co-occurence
library(widyr)

aibo_word_pair <- aibo_toks %>% 
  pairwise_count(word, status_id, sort=TRUE)

aibo_word_cor <- aibo_toks %>% 
  group_by(word) %>% 
  # filter(n() >= 2) %>% 
  pairwise_cor(word, status_id, sort = TRUE) %>% 
  filter(correlation < 0.999) %>%   #if cor=1 means one of the word is used only once
  filter(correlation > 0) # I don't care exclustion of words

hosip_word <- c("入院", "退院", "病院", "病気")
sleepword <- "寝"
riceword <- "ごはん"

aibo_word_cor %>% 
  filter(str_detect(item1, paste(riceword, collapse = "|"))) %>% 
  group_by(item1) %>% 
  top_n(20) %>% 
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

aibo_word_cor %>% 
  mutate(sleep = str_detect(item1, paste(riceword, collapse = "|"))) %>% 
  filter(sleep == TRUE) %>% 
  filter(correlation > .04) %>%
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = F) +
  geom_node_point(color = "#ffff00", size = 5) +
  geom_node_text(aes(label = name), repel= T) +
  theme_void()



## -----------------------------
# TF-IDF
aibo_word <- aibo_toks %>% 
  count(status_id, word, sort=TRUE)

total_word <- aibo_word %>% 
  group_by(status_id) %>% 
  summarise(total = n())

aibo_word <- left_join(aibo_word, total_word)


# Zipf's law
freq_by_rank <- aibo_word %>% 
  group_by(status_id) %>% 
  mutate(rank = row_number(),
         term_frequency = n/total) %>% 
  ungroup()


aibo_tf_idf <- aibo_word %>% 
  bind_tf_idf(word, status_id, n)


aibo_tf_idf %>% 
  select(-total) %>% 
  arrange(desc(tf_idf))

library(forcats)

aibo_tf_idf %>% 
  group_by(status_id) %>% 
  slice_max(tf_idf, )


## -----------------------------
# remove punctuation
punct_list <- aibo_toks %>% 
  mutate(word_len = nchar(word), puncts = str_detect(word, "[[:punct:]]")) %>% 
  filter(puncts == TRUE & word_len < 2) %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  ungroup()

aibo_toks <- aibo_toks %>% 
  anti_join(punct_list, by = "word")


# remove stopwords (Japnaese and English)
data("stop_words")
ja_stopwords <- tibble(word = stopwords::stopwords("ja", "stopwords-iso"))
aibo_list <- tibble(word = c("aibo", "AIBO", "Aibo", "アイボ", "あいぼ"))

aibo_toks <- anti_join(aibo_toks, stop_words, by = "word")
aibo_toks <- anti_join(aibo_toks, ja_stopwords, by = "word")
aibo_toks <- anti_join(aibo_toks, aibo_list, by = "word")


# remove hiragana-only word
gana_list <- aibo_toks %>% 
  mutate(gana = str_detect(word, "^[ぁ-ん]+$")) %>% 
  filter(gana == TRUE) %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  ungroup()

aibo_toks <- anti_join(aibo_toks, gana_list, by = "word")


# refi tokens 
kanji_list <- aibo_toks %>% 
  mutate(kanji = str_detect(word, "^[一-龥]+$")) %>% 
  filter(kanji == TRUE) %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  ungroup()
