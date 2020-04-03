library(tidyverse)
library(lubridate)

theme_set(
  firasans::theme_ipsum_fsc(
    axis_text_size = 10, 
    axis_title_size = 14,
    axis_title_just = "cc") +
    theme(panel.grid.minor = element_blank())
)

dat <- read_rds(here::here("data-snapshots/policy_2020-04-02_2107EST.rds"))

# set NAs to one week from today, which is the assumption 
# commonly made by the IHME team,
dat <- dat %>% mutate_if(is.POSIXct, 
                         replace_na, 
                         replace = lubridate::as_datetime(today() + 7))

gov <- read_rds(here::here("data-snapshots/gov_2020-04-02_2107EST.rds"))

dat %>% 
  left_join(gov, by = "State") %>%
  ggplot() +  
  geom_point(aes(x = `Stay at home/ shelter in place`,
                 y = reorder(State, `Stay at home/ shelter in place`),
                 color = Party), show.legend = FALSE) + 
  scale_color_manual(values = c("#2E74C0", "#CB454A")) +
  geom_vline(xintercept = lubridate::as_datetime(today()), 
             color = "gray50", 
             linetype = "dashed") + 
  xlab("Date of stay at home / shelter in place order") + 
  ylab(element_blank())
