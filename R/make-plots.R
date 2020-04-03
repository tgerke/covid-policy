source(here::here("R", "plot.R"))

plots <- list(
  "state-of-emergency" = plot_action_timeline(dat, `State of emergency`, "State of Emergency declared"),
  "evictions" = plot_action_timeline(dat, `Froze evictions`, "Freeze on evictions"),
  "non-essential" = plot_action_timeline(dat, `Closed non-essential businesses`, "Non-essential business closures"),
  "stay-at-home" = plot_action_timeline(dat, `Stay at home/ shelter in place`, "Stay-at-home orders"),
  "utility-shut-offs" = plot_action_timeline(dat, `Order freezing utility shut offs`, "Freeze on utility shut-offs")
)

purrr::iwalk(plots, save_plot)