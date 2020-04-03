library(tidyverse)
library(lubridate)
library(ggtext)

theme_set(
  firasans::theme_ipsum_fsc(
    axis_text_family = "Fira Sans Condensed",
    axis_text_size = 10, 
    axis_title_size = 14,
    axis_title_just = "cc") +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_markdown(),
          plot.subtitle = element_markdown(),
          plot.caption = element_markdown(),
          plot.title.position = "plot")
)

dat <- read_rds(here::here("data-snapshots/policy_2020-04-03_0848EST.rds"))

# set NAs to one week from today, which is the assumption 
# commonly made by the IHME team,
dat <- dat %>% 
  mutate_if(is.POSIXct, 
            replace_na, 
            replace = lubridate::as_datetime(today() + 7)) 

gov <- read_rds(here::here("data-snapshots/gov_2020-04-02_2107EST.rds"))

party_colors <- c(Democratic = "#2E74C0", Republican = "#CB454A")

date_diff <- dat %>% 
  left_join(gov, by = "State") %>%
  group_by(Party) %>% 
  summarize(mean_date = mean(`Stay at home/ shelter in place`)) %>% 
  mutate(
    date_diff = (mean_date - mean_date[which(Party == "Democratic")])/ddays(1)
  ) %>%
  slice(2) %>%
  pull(3) %>% 
  round(1)

dat %>% 
  mutate(enacted = 
         !`Stay at home/ shelter in place` == lubridate::as_datetime(today() + 7)) %>%
  left_join(gov, by = "State") %>%
  arrange(Party, `Stay at home/ shelter in place`, State) %>% 
  mutate_at(vars(State), forcats::fct_inorder) %>% 
  ggplot() +  
  geom_point(
    aes(x = `Stay at home/ shelter in place`,
        y = State,
        color = Party,
        alpha = enacted),
    show.legend = FALSE) +
  scale_color_manual(values = party_colors) +
  scale_alpha_discrete(range = c(.4, 1)) +
  geom_vline(xintercept = lubridate::as_datetime(today()), 
             color = "gray50", 
             linetype = "dashed") + 
  xlab(element_blank()) + 
  ylab(element_blank()) + 
  geom_curve(
    data = tibble(
      x = c(lubridate::as_datetime(today() - 1.5), 
            lubridate::as_datetime(today() + 6.8)),
      y = c(5, 25),
      xend = c(lubridate::as_datetime(today()),
               lubridate::as_datetime(today() + 5.5)), 
      yend = c(2.5, 24),
      Party = c("Democratic", "Republican")),
    aes(x = x, y = y, xend = xend, yend = yend),
    size = .3,
    curvature = .5,
    color = "gray50", 
    arrow = arrow(length = unit(.1, "cm"))) + 
  geom_text(
    data = tibble(
      x = c(lubridate::as_datetime(today() - 1.5), 
            lubridate::as_datetime(today() + 5)),
      y = c(5.7, 23.5),
      Party = c("Democratic", "Republican"),
      label = c("Today", "Not yet enacted")
    ),
    aes(x = x, y = y, label = label), 
    size = 3,
    color = "gray50") +
  facet_wrap(vars(Party), ncol = 1, scales = "free_y") + 
  labs(
    title = glue::glue(
      "Stay-at-home orders from ",
      "<span style = 'color:{party_colors['Democratic']}'>Democratic</span>",
      " and ",
      "<span style = 'color:{party_colors['Republican']}'>Republican</span>",
      " governors"),
    subtitle = glue::glue(
      "Enacted an average of ",
      "<span style = 'color:{party_colors['Democratic']}'><b>{date_diff}</b></span>",
      " days earlier by ",
      "<span style = 'color:{party_colors['Democratic']}'>Democratic</span> leaders"),
    caption = glue::glue(
      "Data: tinyurl.com/statepolicies<br>",
      "Code: github.com/tgerke/covid-policy<br>",
      "Twitter: @travisgerke"
    )
  ) + 
  theme(
    strip.text = element_blank(),
    panel.spacing.y = unit(0.5, "line")
  )
  
ggsave(here::here("plots", "stay-at-home.png"), 
       device = "png",
       width = 8,
       height = 9,
       units = "in")


