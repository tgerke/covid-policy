library(tidyverse)
library(lubridate)
library(ggtext)

`%||%` <- purrr::`%||%`

theme_set(
  firasans::theme_ipsum_fsc(
    axis_text_family = "Fira Sans Condensed",
    axis_text_size = 10, 
    axis_title_size = 14,
    axis_title_just = "cc") +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_markdown(face = "plain"),
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
            replace = lubridate::as_datetime(today() + 7)) %>% 
  mutate_if(is.POSIXct, as_date)

gov <- read_rds(here::here("data-snapshots/gov_2020-04-02_2107EST.rds"))

ihme <- read_csv(
  here::here("data-snapshots/IHME_Hospitalization_all_locs_2020-04-14.csv")) 
# filter to final date and select cumulative death variables
# ihme <- ihme %>%
#   group_by(location_name) %>% 
#   filter(date == max(date)) %>% 
#   ungroup() %>%
#   rename(State = location_name, deaths_est = totdea_mean) %>% 
#   select(State, deaths_est, totdea_lower, totdea_upper)
# 
# dat <- dat %>% 
#   left_join(ihme, by = "State") %>%
#   mutate(deaths_per_capita = deaths_est/`Population 2018`)

party_colors <- c(Democratic = "#2E74C0", Republican = "#CB454A")

plot_action_timeline <- function(dat, action, action_label = NULL) {
  action <- rlang::enquo(action)
  action_name <- rlang::quo_name(action)
  action_label <- action_label %||% action_name
  
  date_diff <- dat %>% 
    left_join(gov, by = "State") %>%
    group_by(Party) %>% 
    summarize(mean_date = mean(!!action)) %>% 
    mutate(
      date_diff = (mean_date - mean_date[which(Party == "Democratic")])/ddays(1)
    ) %>%
    slice(2) %>%
    pull(3) %>% 
    round(1)
  
  dat <- dat %>% 
    mutate(enacted = !!action != today() + 7) %>% 
    left_join(gov, by = "State") %>%
    arrange(Party, !!action, State) %>% 
    mutate_at(vars(State), forcats::fct_inorder)
  
  all_enacted <- all(dat$enacted)
  
  g <- ggplot(dat) +
    aes(
      x = !!action,
      y = State
    ) +
    scale_color_manual(values = party_colors) +
    scale_alpha_discrete(range = c(.4, 1)) +
    xlab(element_blank()) + 
    ylab(element_blank()) +
    facet_wrap(vars(Party), ncol = 1, scales = "free_y") + 
    labs(
      title = glue::glue(
        "**{action_label}** by ",
        "<strong style = 'color:{party_colors['Democratic']}'>Democratic</strong>",
        " and ",
        "<strong style = 'color:{party_colors['Republican']}'>Republican</strong>",
        " governors"),
      subtitle = glue::glue(
        "Declared an average of ",
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

  date_line_reference <- if (all_enacted) max(dat[[action_name]]) else today()
  
  g <- g +
    geom_vline(
      xintercept = date_line_reference, 
      color = "gray50", 
      linetype = "dashed"
    ) +
    xlim(
      min(dat[[action_name]], na.rm = TRUE), 
      date_line_reference + ifelse(all_enacted, 0, 7)
    )
  
  g <- g + if (!all_enacted) {
    geom_point(
      aes(color = Party,
          alpha = enacted),
          #size = deaths_per_capita),
      show.legend = FALSE)
  } else {
    geom_point(
      aes(x = !!action,
          y = State,
          color = Party),
          #size = deaths_per_capita),
      show.legend = FALSE)
  }
  
  
  g <- g +
    geom_curve(
      data = tibble(
        x = date_line_reference - 1,
        y = 5,
        xend = date_line_reference, 
        yend = 2.5,
        Party = "Democratic"),
      aes(x = x, y = y, xend = xend, yend = yend),
      size = .3,
      curvature = .5,
      color = "gray50", 
      arrow = arrow(length = unit(.1, "cm"))) + 
    geom_label(
      data = tibble(
        x = date_line_reference - 1,
        y = 5.7,
        Party = "Democratic",
        label = if (all_enacted) "Enacted\nin all\nstates" else "Today"
      ),
      aes(x = x, y = y, label = label), 
      size = 3,
      vjust = 0,
      fill = "white",
      label.size = NA,
      color = "gray50")

  if (all_enacted) return(g)
  
  g +
    geom_curve(
      data = tibble(
        x = today() + 6.8,
        y = 25,
        xend = today() + 5.5,
        yend = 24,
        Party = "Republican"),
      aes(x = x, y = y, xend = xend, yend = yend),
      size = .3,
      curvature = .5,
      color = "gray50", 
      arrow = arrow(length = unit(.1, "cm"))) + 
    geom_label(
      data = tibble(
        x = today() + 5,
        y = 23.5,
        Party = "Republican",
        label = "Not yet\nenacted"
      ),
      aes(x = x, y = y, label = label), 
      size = 3,
      vjust = 1,
      fill = "white",
      label.size = NA,
      color = "gray50")
}

save_plot <- function(plot, filename) {
  ggsave(
    here::here("plots", glue::glue("{filename}.png")),
    plot,
    device = "png",
    width = 8,
    height = 9,
    units = "in"
  )
}
