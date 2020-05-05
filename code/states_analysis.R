library(tidyverse)
library(tidytext)
library(geofacet)
library(patchwork)

stories <- read_rds(here::here("data", "cleaned_text", 
                               "states_full.rds")) %>%
  filter(nchar(new_text) > 300)

lexicon_bing <- get_sentiments("bing")
lexicon_loughran <- get_sentiments("loughran")
lexicon_nrc <- get_sentiments("nrc")

stories_unnest <- stories %>%
  unnest_tokens(word, new_text) %>%
  filter(!word %in% c("john", "march", "pay", "content", "subscribe", 
                      "tribune", "sun", "press", "associated", "phoenix",
                      "vice"))

stories_bing <- stories_unnest %>% 
  inner_join(lexicon_bing) %>%
  filter(sentiment %in% c("negative", "positive")) %>%
  select(stories_id, date, media_name, state, media_type, 
         word, sentiment_bing = sentiment)

stories_nrc <- stories_unnest %>% 
  inner_join(lexicon_nrc) %>%
  filter(sentiment %in% c("negative", "positive")) %>%
  select(stories_id, date, media_name, state, media_type, 
         word, sentiment_nrc = sentiment)

stories_loughran <- stories_unnest %>% 
  inner_join(lexicon_loughran) %>%
  filter(sentiment %in% c("negative", "positive")) %>%
  select(stories_id, date, media_name, state, media_type, 
         word, sentiment_loughran = sentiment)

stories_all <- stories_bing %>%
  full_join(stories_nrc) %>%
  full_join(stories_loughran)


polarity <- stories_all %>%
  group_by(stories_id, date, media_name, state, media_type) %>%
  summarise_at(vars(starts_with("sentiment")), 
               list(positive = ~mean(. == "positive", na.rm = TRUE),
                    negative = ~mean(. == "negative", na.rm = TRUE),
                    polarity = ~mean(. == "positive", na.rm = TRUE) -
                      mean(. == "negative", na.rm = TRUE)
               )
  ) %>%
  ungroup()

polarity_long <- polarity %>%
  pivot_longer(cols = ends_with("polarity"),
               names_pattern = "sentiment\\_(.*)\\_polarity",
               names_to = "lexicon",
               values_to = "polarity") %>%
  select(-starts_with("sentiment"))


states_graph <- function(lex) {
  p <- polarity_long %>%
    filter(!is.na(polarity), lexicon == lex,
           between(date, lubridate::ymd("2020-02-01"), 
                   lubridate::ymd("2020-05-01"))
    ) %>%
    group_by(date, state) %>%
    mutate(avg = mean(polarity)) %>%
    ungroup() %>%
    ggplot(aes(date, polarity, col = state)) +
    geom_line(aes(col = state), alpha = 0.25) +
    geom_smooth(col = "black", se = FALSE, size = .25) +
    theme_void() +
    theme(legend.position = "none") +
    labs(y = "Polarity score",
         x = "Date") +
    facet_geo(vars(state)) +
    ggtitle(glue::glue("State news polarity scores Feb-April using {lex} lexicon"))
  ggsave(here::here("figures", 
                    glue::glue("polarity by state over time {lex}.pdf")),
         p, height = 10, width = 12)
}

states_graph("bing")
states_graph("nrc")
states_graph("loughran")

source_graph <- function(lex) {
  p <- count(polarity, media_name) %>%
    top_n(16, wt = n) %>%
    left_join(polarity_long, by = "media_name") %>%
    filter(!is.na(polarity), lexicon == lex,
           between(date, lubridate::ymd("2020-02-01"), 
                   lubridate::ymd("2020-05-01"))
    ) %>%
    group_by(date, media_name) %>%
    mutate(avg = mean(polarity)) %>%
    ungroup() %>%
    ggplot(aes(date, polarity, col = media_name)) +
    geom_line(aes(col = state), alpha = 0.25) +
    geom_smooth(col = "black", se = FALSE, size = .25) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(y = "Polarity score",
         x = "Date") +
    facet_wrap(vars(media_name)) +
    ggtitle(glue::glue("Polarity scores by source Feb-April using {lex} lexicon"),
            subtitle = "Top 16 news sources")
  ggsave(here::here("figures", 
                    glue::glue("polarity by source over time {lex}.pdf")),
         p, height = 10, width = 12)
}

source_graph("bing")
source_graph("nrc")
source_graph("loughran")


stories <- stories %>%
  left_join(polarity)

write_rds(stories, here::here("data", "cleaned_text", "states_polarity.rds"))
write_csv(stories, here::here("data", "cleaned_text", "states_polarity.csv"))
