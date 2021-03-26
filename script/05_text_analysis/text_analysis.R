## import and extract text data
md <- rtweet::read_twitter_csv(here::here("data/clean_data/202005-08/all_clean_20201104_v1.csv"))


# filter out retweets, and select necessary variables
library(dplyr)

md <- as.data.frame(md) %>% 
  filter(is_retweet == "FALSE") %>% 
  mutate(post_date = lubridate::date(created_at)) %>% 
  select(status_id, screen_name, post_date, is_aibo, is_lovot, text) %>% 
  rename(raw_text = text)



## clean text: removing URL, mentions and emoji
library(stringr)

md <- md %>% 
  mutate(text = str_replace_all(raw_text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp", " ") %>%  #remove URL
           str_replace_all("#[[:alnum:]_]+", " ") %>%  #remove hashtag
           str_replace_all("@[[:alnum:]_]+", " ") %>%  #remove mention
           str_replace_all("<.[^ぁ-んァ-ン一-龯]*>|\n", " ") %>% #remove \n and some emoji
           str_squish()) %>% #remove extra space
  filter(nchar(text) > 0) #filter out empty text

# remove 仮面ライダー tweets
rider_list <- c("仮面ライダー", "ゼロワン", "ヒューマギア", "城之内", "飛電")
md <- md %>% 
  mutate(rider = str_detect(text, paste(rider_list, collapse = "|"))) %>%
  filter(rider==FALSE)


## covert to corpus
library(quanteda)
quanteda_options("threads" = 4)

aibo_corp <- filter(md, is_aibo == TRUE) %>% corpus()
lovot_corp <- filter(md, is_lovot == TRUE) %>% corpus()


## morphological analysis
library(RMeCab)

aibo_toks <- texts(aibo_corp) %>% 
  purrr::map(function(x) unlist(RMeCabC(x, dic = "mecab-dic/mecab-user-dict-seed.20200910.csv.dic"))) %>% 
  as.tokens()
  
lovot_toks <- texts(lovot_corp) %>% 
  purrr::map(function(x) unlist(RMeCabC(x, dic = "mecab-dic/mecab-user-dict-seed.20200910.csv.dic"))) %>% 
  as.tokens()

## Refining tokens
aibo_toks_refi <- aibo_toks

# refi kanji
tstat_kanji <- aibo_toks %>% 
  tokens_select("^[一-龥]+$", valuetype = "regex", padding = TRUE) %>% 
  textstat_collocations(min_count = 5, tolower = TRUE)

aibo_toks_refi <- tokens_compound(aibo_toks_refi, tstat_kanji[tstat_kanji$z > 2],
                                  concatenator = "", join = TRUE)

# refi katakana
tstat_kana <- aibo_toks_refi %>% 
  tokens_select("^[ァ-ンー]+$", valuetype = "regex", padding = TRUE) %>% 
  textstat_collocations(min_count = 5, tolower = TRUE)

aibo_toks_refi <- tokens_compound(aibo_toks_refi, tstat_kana[tstat_kana$z > 2],
                                  concatenator = "", join = TRUE)

# refi other combination
tstat_any <- aibo_toks_refi %>% 
  tokens_select("^[0-9ァ-ンー一-龥]+$", valuetype = "regex", padding = TRUE) %>% 
  textstat_collocations(min_count = 10, tolower = TRUE)

aibo_toks_refi <- tokens_compound(aibo_toks_refi, tstat_any[tstat_any$z > 2],
                                  concatenator = "", join = TRUE)

aibo_toks_refi <- tokens(aibo_toks_refi, remove_punct = TRUE) #remove punctuation
aibo_toks_refi <- tokens_remove(aibo_toks_refi, pattern = stopwords("ja", source = "stopwords-iso")) # remove stopwords
aibo_toks_refi <- tokens_remove(aibo_toks_refi, "^[ぁ-ん]+$", valuetype = "regex") #remove hiragana-only terms


lovot_toks_refi <- tokens(lovot_toks, remove_punct = TRUE) %>% 
  tokens_remove(pattern = "^[ぁ-ん]+$", valuetype = "regex")

lovot_tstat <- lovot_toks %>% 
  textstat_collocations(min_count = 5, tolower = FALSE)

lovot_toks_refi <- tokens_compound(lovot_toks_refi, lovot_tstat[lovot_tstat$z > 2],
                                   concatenator = "", join = TRUE)

lovot_toks <- tokens_remove(lovot_toks, pattern = stopwords("ja", source = "stopwords-iso"))

## convert to DFM
aibo_dfm <- aibo_toks_refi %>% 
  tokens_remove("") %>% 
  dfm(tolower = FALSE) %>% 
  dfm_select(min_nchar = 2) %>% #filter out single-character-only word
  dfm_trim(min_docfreq = 10, max_docfreq = Inf)

aibo_dfm_bin <- aibo_toks_refi %>%   # binary matrix: for co-occurrence
  tokens_remove("") %>% 
  dfm(tolower = FALSE) %>% 
  dfm_select(min_nchar = 2) %>% #filter out single-character-only word
  dfm_trim(min_docfreq = 10, max_docfreq = Inf) %>% 
  dfm_weight("boolean")

lovot_dfm <- dfm(lovot_toks_refi, tolower = FALSE) %>% 
  dfm_select(min_nchar = 2) %>% 
  dfm_trim(min_termfreq = 10)


#check top feature
aibo_list <- c("aibo", "AIBO", "Aibo", "アイボ")
lovot_list <- c("lovot", "LOVOT", "Lovot", "ラボット")

aibo_dfm <- aibo_dfm %>% 
  dfm_select(pattern = aibo_list, selection = "remove")

aibo_dfm_bin <- aibo_dfm_bin %>% 
  dfm_select(pattern = aibo_list, selection = "remove")

topfeatures(aibo_dfm_bin, 50)

lovot_dfm %>% 
  dfm_select(pattern = lovot_list, selection = "remove") %>% 
  topfeatures(100)

summary(aibo_dfm)
summary(lovot_dfm)



## visualization
library(ggplot2)

aibo_p <- aibo_dfm %>% 
  # dfm_select(pattern = aibo_list, selection = "remove") %>% 
  textstat_frequency(n=50) %>% 
  ggplot(aes(x=reorder(feature, frequency), y=frequency)) +
  geom_point() +
  coord_flip() +
  theme_minimal() +
  labs(x = "Top 50 Terms", y = "Frequency (AIBO)")

lovot_p <- lovot_dfm %>% 
  dfm_select(pattern = lovot_list, selection = "remove") %>% 
  textstat_frequency(n=50) %>% 
  ggplot(aes(x=reorder(feature, frequency), y=frequency)) +
  geom_point() +
  coord_flip() +
  theme_minimal() +
  labs(x = "", y = "Frequency (LOVOT)")

see::plots(aibo_p, lovot_p, n_columns = 2)

textplot_wordcloud(aibo_dfm, max_words = 50, color = "skyblue")
textplot_wordcloud(lovot_dfm, max_words = 50, color= "palegreen")


## Term Co-Occurence 
aibo_fcm <- fcm(aibo_dfm)
lovot_fcm <- fcm(lovot_dfm)

aibo_top  <-  topfeatures(aibo_fcm, 50) %>%   #top N features
  names()

lovot_top <- fcm_select(lovot_fcm, pattern = lovot_list, selection = "remove") %>% 
  topfeatures(50) %>% 
  names()

aibo_size <- dfm_select(aibo_dfm, aibo_top) %>% 
  colSums() %>% 
  log()

lovot_size <- dfm_select(lovot_dfm, lovot_top) %>% 
  colSums() %>% 
  log() 

set.seed(234)
aibo_fcm %>% fcm_select(pattern = aibo_top) %>% 
  textplot_network(min_freq = 0.5, vertex_size = aibo_size / max(aibo_size) * 3, edge_alpha = 0.3)

lovot_fcm %>% fcm_select(pattern = lovot_top) %>% 
  textplot_network(min_freq = 0.5, vertex_size = lovot_size / max(lovot_size) * 3, edge_alpha = 0.3)

