library(tidyverse)
library(lubridate)

dat <- read_rds(here::here("data-snapshots/policy_2020-04-02_14:55EST.rds"))

# set NAs to one week from today, which is the assumption 
# commonly made by the IHME team,
dat <- dat %>% mutate_if(is.POSIXct, 
                         replace_na, 
                         replace = lubridate::as_datetime(today() + 7))

View(dat)
