library(here)
library(rtweet)
library(dplyr)
library(ggplot2)
library(CINNA)
library(kableExtra)

# import data
md <- rtweet::read_twitter_csv(here("data/clean_data/202005-08/all_clean_20201104_v2.csv"))


# tweets of aibo/lovot
md %>% 
  mutate(robots = is_aibo + is_lovot) %>% 
  mutate(robots = ifelse(is_lovot==0, 0, robots)) %>% 
  mutate(types = recode(robots, `0`="aibo_only", `1`="lovot_only", `2`="both")) %>% 
  group_by(types) %>% 
  summarise(n = n()) %>% 
  mutate(p = round(n/sum(n)*100, 1)) %>% 
  arrange(desc(n)) %>% 
  rename("Robot"="types", "Count"=n, "Perct"=p) %>% 
  janitor::adorn_totals() %>% 
  kbl() %>% 
  kable_styling(full_width = F, position = "left",
                bootstrap_options = "striped") %>% 
  column_spec(1, bold=T) %>% 
  row_spec(4, bold=T)


# number of tweets (and the proportion of retweet)
aibo <- md %>% 
  filter(is_aibo == TRUE) %>% 
  group_by(is_retweet) %>% 
  summarise(n = n()) %>% 
  mutate(p = round(n/sum(n)*100, 1)) %>% 
  rename("Count (aibo)" = n, "% (aibo)" = p)

lovot <- md %>% 
  filter(is_lovot == TRUE) %>% 
  group_by(is_retweet) %>% 
  summarise(n = n()) %>% 
  mutate(p = round(n/sum(n)*100, 1)) %>% 
  rename("Count (lovot)" = n, "% (lovot)" = p)

full_join(aibo, lovot) %>% 
  mutate(is_retweet = if_else(is_retweet == TRUE, "Retweet", "Tweet")) %>% 
  # mutate(is_retweet = recode(is_retweet, `0`="Tweet", `1`="Retweet")) %>% 
  rename("Tweet Type" = is_retweet) %>% 
  janitor::adorn_totals() %>% 
  kbl() %>% 
  kable_styling(full_width = F, bootstrap_options = "striped") %>% 
  column_spec(1, bold=T) %>% 
  row_spec(3, bold=T)


# hashtags
aibo_hash <- md %>% 
  filter(is_aibo == TRUE & is_retweet == FALSE) %>% 
  filter(hashtags != "NA") %>% 
  mutate(hashtags = strsplit(unlist(hashtags), " ")) %>% 
  tidyr::unnest(hashtags) %>% 
  group_by(tolower(hashtags)) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  ungroup() %>% 
  rename("Hashtag" = `tolower(hashtags)`)

ggplot(aibo_hash[1:50,], aes(x=reorder(Hashtag, Count), y=Count)) +
  geom_col(fill="grey") +
  coord_flip() +
  labs(x="Top 50 Hashtags", title = "Aibo (without retweets)") +
  theme_lucid()


lovot_hash <- md %>% 
  filter(is_lovot == TRUE & is_retweet == FALSE) %>% 
  filter(hashtags != "NA") %>% 
  mutate(hashtags = strsplit(unlist(hashtags), " ")) %>% 
  tidyr::unnest(hashtags) %>% 
  group_by(tolower(hashtags)) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  ungroup() %>% 
  rename("Hashtag" = `tolower(hashtags)`)

ggplot(lovot_hash[1:50,], aes(x=reorder(Hashtag, Count), y=Count)) +
  geom_col(fill="grey") +
  coord_flip() +
  labs(x="Top 50 Hashtags", title = "Lovot (without retweets)") +
  theme_lucid()


# tweet having hashtags
aibo <- md %>% 
  filter(is_aibo == TRUE & is_retweet == FALSE) %>% 
  mutate(has_tags = if_else(hashtags != "NA", "Yes", "No", "No")) %>% 
  group_by(has_tags) %>% 
  summarize(n = n()) %>% 
  mutate(p = round(n/sum(n)*100, 1)) %>% 
  ungroup() %>% 
  rename(`Count (aibo)`=n, `% (aibo)`=p)

lovot <- md %>% 
  filter(is_lovot == TRUE & is_retweet == FALSE) %>% 
  mutate(has_tags = if_else(hashtags != "NA", "Yes", "No", "No")) %>% 
  group_by(has_tags) %>% 
  summarize(n = n()) %>% 
  mutate(p = round(n/sum(n)*100, 1)) %>% 
  ungroup() %>% 
  rename(`Count (lovot)`=n, `% (lovot)`=p)

full_join(aibo, lovot, by = "has_tags") %>% 
  janitor::adorn_totals() %>% 
  rename("Has Hashtag" = has_tags) %>% 
  kbl() %>% 
  kable_styling(full_width = F, position = "left",
                bootstrap_options = "striped") %>% 
  column_spec(1, bold=T) %>% 
  column_spec(4:5, background = "#ffffe6") %>% 
  row_spec(3, bold=T)


# network data
library(igraph)
library(ggraph)

re_lovot <- md %>% 
  filter(is_retweet==F & is_lovot == TRUE) %>% 
  filter(reply_to_user_id != "NA") %>% 
  select(screen_name, reply_to_screen_name) %>% 
  tidyr::unnest(reply_to_screen_name) %>% 
  filter(!is.na(reply_to_screen_name)) %>% 
  graph_from_data_frame()

pp <- giant_component_extract(re_lovot)

ggplot(tibble(y=degree_distribution(re_lovot), x=1:length(y))) +
  geom_segment(aes(x, y, xend=x, yend=0), color="slateblue") +
  scale_y_continuous(expand = c(0,0), trans = "sqrt") +
  labs(x="Degree", y="Density (sqrt scale)", title="Lovot Reply Degree Distribution") +
  theme_minimal()

V(pp)$node_label <- unname(ifelse(degree(pp)[V(pp)] > 100, names(V(pp)), ""))
V(pp)$node_size <- unname(ifelse(degree(pp)[V(pp)] > 100, degree(pp), 0))

ggraph(re_lovot, layout = "fr") + 
  geom_edge_arc(edge_width=0.05, aes(alpha=..index..)) +
  geom_node_label(aes(label=node_label, size=node_size),
                  label.size=0, fill="#ffffff66", segment.colour="springgreen",
                  color="slateblue", repel=TRUE) +
  coord_fixed() +
  scale_size_area(trans="sqrt") +
  # scale_edge_alpha("Direction", guide="edge_direction") +
  labs(title="Reply Relationship", subtitle="Most replied names labeled (>100). Darkers edges: got more replies. Label size: larger degree") +
  theme_void()


#--
re_aibo <- md %>% 
  filter(is_retweet==F & is_aibo == TRUE) %>% 
  filter(reply_to_user_id != "NA") %>% 
  select(screen_name, reply_to_screen_name) %>% 
  tidyr::unnest(reply_to_screen_name) %>% 
  filter(!is.na(reply_to_screen_name)) %>% 
  graph_from_data_frame()

kk <- giant_component_extract(re_aibo)

ggplot(tibble(y=degree_distribution(re_aibo), x=1:length(y))) +
  geom_segment(aes(x, y, xend=x, yend=0), color="slateblue") +
  scale_y_continuous(expand = c(0,0), trans = "sqrt") +
  labs(x="Degree", y="Density (sqrt scale)", title="Aibo Reply Degree Distribution") +
  theme_minimal()

V(re_aibo)$node_label <- unname(ifelse(degree(re_aibo)[V(re_aibo)] > 200, names(V(re_aibo)), ""))
V(re_aibo)$node_size <- unname(ifelse(degree(re_aibo)[V(re_aibo)] > 200, degree(re_aibo), 0))

ggraph(re_aibo, layout = "kk") + 
  geom_edge_arc(edge_width=0.05, aes(alpha=..index..)) +
  geom_node_label(aes(label=node_label, size=node_size),
                  label.size=0, fill="#ffffff66", segment.colour="springgreen",
                  color="slateblue", repel=TRUE) +
  coord_fixed() +
  scale_size_area(trans="sqrt") +
  scale_edge_alpha("Direction", guide="edge_direction") +
  labs(title="Reply Relationship", subtitle="Most replied names labeled (>100). Darkers edges: got more replies. Label size: larger degree") +
  theme_void()
