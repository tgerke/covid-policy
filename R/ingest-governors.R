library(rvest)
library(tidyverse)

# read Governor party affliations from Wiki
gov <- read_html("https://en.wikipedia.org/wiki/List_of_United_States_governors") %>%
  html_table(fill = TRUE)
gov <- gov[[1]] %>%
  janitor::row_to_names(row_number = 1) %>%
  janitor::clean_names() %>% 
  select(state, party_2) %>%
  rename(State = state, Party = party_2) %>%
  mutate(Party = recode(Party,
                        "Democratic–Farmer–Labor" = "Democratic",
                        "Republican[note 1]" = "Republican"
  ))

readr::write_rds(gov,
                 here::here("data-snapshots", 
                            glue::glue("gov_", 
                                       format(Sys.time(), "%Y-%m-%d_%H%MEST"),
                                       ".rds")))
