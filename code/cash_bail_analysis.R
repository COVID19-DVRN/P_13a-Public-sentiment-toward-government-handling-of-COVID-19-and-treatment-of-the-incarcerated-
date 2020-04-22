library(tidyverse)
library(tidytext)
library(geofacet)

stories <- read_rds(here::here("data", "cash_bail_full.rds")) %>%
  filter(
    # !stories_id %in% c(1546351676, 1555128457, 1558662816, 1566033085, 1566988842,
    #                        1571735542, 1572010835, 1576223278),
         nchar(new_text) > 250)

nrc <- get_sentiments("nrc")

stories_unnest <- stories %>%
  unnest_tokens(word, new_text) %>%
  filter(!word %in% c("john", "march", "pay", "content", "subscribe", 
                      "tribune", "sun", "press", "associated", "phoenix",
                      "vice")) %>%
  inner_join(nrc) %>%
  select(stories_id, date, media_name, pub_state, media_type, word, sentiment)

counted <- stories_unnest %>%
  group_by(stories_id, date, media_name, pub_state, media_type) %>%
  count(sentiment) %>%
  mutate(total = sum(n), prop = n / total) %>%
  ungroup()


counted_wide <- counted %>%
  select(-n) %>%
  pivot_wider(names_from = sentiment, values_from = prop) %>%
  replace_na(list(anger = 0, anticipation = 0, disgust = 0, fear = 0, joy = 0, 
                  negative = 0, positive = 0, sadness = 0, surprise = 0, trust = 0))

for (i in 2:7) {
  k <- kmeans(select(counted_wide, anger:trust), i, iter.max = 100)
  counted_wide[[paste0("k", i)]] <- k$cluster
}

by_cluster <- counted_wide %>%
  pivot_longer(starts_with("k"), names_to = "k", values_to = "cluster") %>%
  mutate(k = parse_number(k) %>% paste(., "clusters"))

ggplot(by_cluster, aes(positive, negative, col = factor(cluster))) +
  geom_point() +
  facet_wrap(vars(k)) +
  louisahstuff::my_theme() +
  louisahstuff::my_scale_color(type = "qual", data = "discrete") +
  theme(legend.position = "none") +
  labs(y = "proportion negative words", x = "proportion positive words")

all_dat <- counted_wide %>%
  left_join(stories) %>%
  mutate(group = paste0(k2, k3, k4, k5), # just to see diff combos
         state = str_remove(pub_state, "US\\-"),
         k2_cat = factor(k2, labels = c("negative", "positive")))

all_dat %>%
  filter(!is.na(state), !is.na(k2)) %>%
  ggplot(aes(k2, y = ..prop.., fill = factor(..x..))) +
  geom_bar() +
  coord_flip() +
  theme_void() +
  theme(legend.position = "top") +
  louisahstuff::my_scale_fill(type = "qual", data = "discrete",
                              name = "category", labels = c("negative", "positive")) +
  facet_geo(vars(state))

all_dat %>%
  filter(!is.na(media_type), media_type != "other", !is.na(k2)) %>%
  ggplot(aes(k2, y = ..prop.., fill = factor(..x..))) +
  geom_bar() +
  coord_flip() +
  theme_void() +
  theme(legend.position = "top") +
  louisahstuff::my_scale_fill(type = "qual", data = "discrete",
                              name = "category", labels = c("negative", "positive")) +
  facet_wrap(vars(media_type))

all_dat %>%
  filter(!is.na(k2), !is.na(date)) %>%
  mutate(yday = lubridate::yday(date),
         year = lubridate::year(date)) %>%
  group_by(date, year, yday) %>%
  mutate(n = n()) %>%
  group_by(date, k2_cat, year, yday) %>%
  summarise(prop = n() / max(n)) %>%
  ggplot(aes(yday, y = prop, fill = k2_cat)) +
  geom_col(position = position_dodge()) +
  facet_wrap(vars(year)) + 
  louisahstuff::my_theme() +
  theme(legend.position = "top") +
  louisahstuff::my_scale_fill(type = "qual", data = "discrete", name = "category") +
  labs(y = "Proportion of stories in cluster",
       x = "Date", title = "Clusters over time")

words_w_stories <- stories_unnest %>%
  select(stories_id, word) %>%
  distinct() %>%
  left_join(counted_wide) %>%
  mutate(k2_cat = factor(k2, labels = c("negative", "positive")))

words_w_stories %>%
  group_by(k2_cat) %>%
  mutate(total = n()) %>%
  count(total, word, k2_cat, sort = TRUE) %>%
  mutate(n = n / total) %>%
  group_by(k2_cat) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = k2_cat)) +
  geom_col() +
  facet_wrap(vars(k2_cat), scales = "free_y") +
  labs(y = "Proportion of total words in cluster",
       x = NULL,
       title = "Top 10 words in each cluster") +
  coord_flip() +
  louisahstuff::my_theme() +
  theme(legend.position = "top") +
  louisahstuff::my_scale_fill(type = "qual", data = "discrete",
                              name = "category")
  

top100 <- words_w_stories %>%
  group_by(k2_cat) %>%
  mutate(total = n()) %>%
  count(total, word, k2_cat) %>%
  mutate(prop = n / total) %>%
  group_by(k2_cat) %>%
  arrange(prop) %>%
  top_n(100)

with(filter(top100, k2_cat == "negative"), 
     wordcloud::wordcloud(word, prop, rot.per = 0, fixed.asp = FALSE, scale = c(3, .2), 
                     colors = RColorBrewer::brewer.pal(2, "Dark2")[1]))
with(filter(top100, k2_cat == "positive"), 
     wordcloud::wordcloud(word, prop, rot.per = 0, fixed.asp = FALSE, scale = c(3, .2), 
                          colors = RColorBrewer::brewer.pal(2, "Dark2")[2]))

for_comparison <- words_w_stories %>%
  select(word, k2_cat) %>%
  count(word, k2_cat) %>%
  pivot_wider(names_from = "k2_cat", values_from = "n") %>%
  select(word, negative, positive) %>%
  replace_na(list(negative = 0, positive = 0))

mat <- as.matrix(for_comparison[,-1])
rownames(mat) <- pull(for_comparison, "word")

wordcloud::comparison.cloud(mat, scale = c(3, .2),
                            colors = RColorBrewer::brewer.pal(2, "Dark2"))
