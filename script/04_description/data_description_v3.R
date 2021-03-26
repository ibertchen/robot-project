library(rtweet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(kableExtra)
library(tidytext)
library(janitor)

md <- read_twitter_csv("data/clean_data/202005-08/all_clean_20210105_v3.csv") %>% 
  mutate(robot = forcats::fct_relevel(robot, c("aibo", "lovot", "both")))
md_nort <- filter(md, !is_retweet==TRUE)

# frequency by robot type
md %>% 
  tabyl(robot) %>% 
  adorn_totals("row") %>% 
  adorn_pct_formatting() %>% 
  kable(caption="Tweets by Robot", align = c("lrr")) %>% # align: assigning column value position
  kable_paper(full_width = F, position = "left") %>% 
  column_spec(3, italic = T) %>% 
  row_spec(4, bold=T) %>% 
  footnote(symbol = "both: aibo and lovot were mentioned in the tweet")

# frequency by robot, retweet
md %>% 
  tabyl(robot, is_retweet) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>%  #position="front": put count before percentage
  rename("tweet"="FALSE", "retweet"="TRUE") %>% 
  kable(caption = "Tweet Type by Robot", align = c("lrrr")) %>% 
  kable_paper(full_width = F, position = "left") %>% 
  row_spec(4, bold = T) %>% 
  footnote(symbol = "value: count (row percentage)")
  
library(treemapify)
md %>% 
  count(robot, is_retweet) %>% 
  mutate(`tweet type` = if_else(is_retweet==T, "retweet", "tweet")) %>% 
  ggplot(aes(area=n, label=`tweet type`, subgroup=robot)) +
  geom_treemap(aes(alpha=`tweet type`), fill="#00acee") +
  geom_treemap_text(place="topleft", color="white", grow = T) +
  geom_treemap_subgroup_border(color="orange", size=3) +
  geom_treemap_subgroup_text(place="centre", color="orange") +
  theme(legend.position = "NA")


# hashtag usage
md_nort %>% 
  mutate(with_hashtag = if_else(!hashtags=="NA", "yes", "no", "no")) %>% 
  tabyl(robot, with_hashtag) %>% 
  adorn_totals(c("row","col")) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  select(robot, yes, no, Total) %>% 
  kable(caption = "Tweet with Hashtag", align=c("lrrr")) %>% 
  kable_paper(full_width = F, position = "left") %>% 
  row_spec(4, bold=T) %>% 
  footnote(symbol = c("value: count (row percentage)",
                      "retweets are not counted"))


# popular hashtags
tags <- md_nort %>% 
  filter(!hashtags=="NA") %>% 
  select(robot, hashtags) %>% 
  unnest_tokens(word, hashtags, token = stringr::str_split, pattern=" ") %>% 
  count(robot, word, sort=T) %>% 
  filter(n >=5) %>% 
  group_by(robot) %>% 
  top_n(10, n) %>%
  ungroup() %>% 
  arrange(n) %>% 
  mutate(rank = row_number())
  
tags %>% 
  ggplot(aes(x=n, y=as.factor(rank), fill=robot)) +
  geom_col(show.legend = F) +
  facet_wrap(~forcats::fct_relevel(robot, "aibo","lovot","both"),
             scales = "free") +
  scale_y_discrete(breaks = as.factor(tags$rank),
                   labels = tags$word) +
  labs(x = "Frequency of Hashtags (N≧5)",
       y = "Top 10 Hashtags") +
  theme_bw()


# interaction type

## quote tweet
md_nort %>% 
  tabyl(robot, is_quote) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  rename("yes"="TRUE", "no"="FALSE") %>% 
  select(robot, yes, no, Total) %>% 
  kable(caption = "Number of Quote Tweet", align=c("lrrr")) %>% 
  kable_paper(full_width = F, position = "left") %>% 
  row_spec(4, bold=T) %>% 
  footnote(symbol = c("value: count (row percentage)",
                      "retweets are not counted"))


## reply tweet
md_nort %>% 
  mutate(is_reply = if_else(reply_to_status_id=="NA", "no", "yes", "no")) %>% 
  tabyl(robot, is_reply) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  select(robot, yes, no, Total) %>% 
  kable(caption = "Number of Reply Tweet", align=c("lrrr")) %>% 
  kable_paper(full_width = F, position = "left") %>% 
  row_spec(4, bold=T) %>% 
  footnote(symbol = c("value: count (row percentage)",
                      "retweets are not counted"))
  
## mention tweet
md_nort %>% 
  mutate(is_mention = if_else(mentions_user_id=="NA", "no", "yes", "no")) %>% 
  tabyl(robot, is_mention) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  select(robot, yes, no, Total) %>% 
  kable(caption = "Number of Mention Tweet", align=c("lrrr")) %>% 
  kable_paper(full_width = F, position = "left") %>% 
  row_spec(4, bold=T) %>% 
  footnote(symbol = c("value: count (row percentage)",
                      "retweets are not counted"))


## compare three interaction types 
left_join(md_nort %>% 
            tabyl(robot, is_quote) %>% 
            adorn_percentages() %>% 
            adorn_pct_formatting(affix_sign = F) %>% 
            rename("quote"="TRUE") %>% 
            select(-"FALSE"),
          md_nort %>% 
            mutate(is_reply = if_else(reply_to_status_id=="NA", "no", 
                                      "reply", "no")) %>% 
            tabyl(robot, is_reply) %>% 
            adorn_percentages() %>% 
            adorn_pct_formatting(affix_sign = F) %>% 
            select(-no)) %>% 
  left_join(md_nort %>% 
              mutate(is_mention = if_else(mentions_user_id=="NA", "no", 
                                          "mention", "no")) %>% 
              tabyl(robot, is_mention) %>% 
              adorn_percentages() %>% 
              adorn_pct_formatting(affix_sign = F) %>% 
              select(-no)) %>% 
  pivot_longer(-robot, names_to="types", values_to="percentage") %>% 
  mutate(types = forcats::fct_relevel(types, c("quote", "reply", "mention"))) %>% 
  ggplot(aes(x=as.numeric(percentage), y=robot, fill=robot)) +
  geom_col(show.legend = F) +
  facet_wrap(~types, ncol=1) +
  labs(x="% of Tweets (excluding retweets)", y="Robot") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 50)) +
  theme_bw()

