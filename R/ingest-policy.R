googlesheets4::sheets_deauth()

dat <- googlesheets4::read_sheet("1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ",
                                 n_max = 50, na = "0")

readr::write_rds(dat,
        here::here("data-snapshots", 
                   glue::glue("policy_", 
                              format(Sys.time(), "%Y-%m-%d_%H%MEST"),
                              ".rds")))
