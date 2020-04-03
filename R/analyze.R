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
          plot.title.position = "plot")
)

dat <- read_rds(here::here("data-snapshots/policy_2020-04-03_0848EST.rds"))

# set NAs to one week from today, which is the assumption 
# commonly made by the IHME team,
dat <- dat %>% mutate_if(is.POSIXct, 
                         replace_na, 
                         replace = lubridate::as_datetime(today() + 7))

gov <- read_rds(here::here("data-snapshots/gov_2020-04-02_2107EST.rds"))

party_colors <- c(Democratic = "#2E74C0", Republican = "#CB454A")

dat %>% 
  left_join(gov, by = "State") %>%
  arrange(Party, `Stay at home/ shelter in place`, State) %>% 
  mutate_at(vars(State), forcats::fct_inorder) %>% 
  ggplot() +  
  geom_point(
    aes(x = `Stay at home/ shelter in place`,
        y = State,
        color = Party), 
    show.legend = FALSE) +
  scale_color_manual(values = party_colors) +
  geom_vline(xintercept = lubridate::as_datetime(today()), 
             color = "gray50", 
             linetype = "dashed") + 
  xlab(element_blank()) + 
  ylab(element_blank()) + 
  geom_curve(
    data = tibble(
      x = lubridate::as_datetime(today() - 1.5),
      y = 5,
      xend = lubridate::as_datetime(today()), 
      yend = 2.5,
      Party = "Democratic"),
    aes(x = x, y = y, xend = xend, yend = yend),
    size = .3,
    curvature = .5,
    color = "gray50", 
    arrow = arrow(length = unit(.1, "cm"))) + 
  geom_text(
    data = tibble(
      x = lubridate::as_datetime(today() - 1.5),
      y = 5.7,
      Party = "Democratic"
    ),
    aes(x = x, y = y, label = "Today"), 
    size = 3,
    color = "gray50") +
  facet_wrap(vars(Party), ncol = 1, scales = "free_y") + 
  labs(title = glue::glue(
    "COVID19 action timeline from ",
    "<span style = 'color:{party_colors['Democratic']}'>Democratic</span>",
    " and ",
    "<span style = 'color:{party_colors['Republican']}'>Republican</span>",
    " governors")) + 
  theme(
    strip.text = element_blank(),
    panel.spacing.y = unit(0.5, "line")
  )
  