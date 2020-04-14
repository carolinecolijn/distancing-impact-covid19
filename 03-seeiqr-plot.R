# # start by sourcing `02-seeiqr-fit.R`
#
# library(ggplot2)
# library(dplyr)
# theme_set(theme_light())
# source("make_quick_plots.R")
# setwd(here::here("selfIsolationModel", "stan"))
# dir.create("figs", showWarnings = FALSE)
# .today <- lubridate::today()
#
# # make_quick_plots(m[[1]], id = paste0("-nb2-", .today))
# # make_quick_plots(m[[2]], id = paste0("-nb2-f-1.0-forecast-", .today))
# # make_quick_plots(m[[3]], id = paste0("-nb2-0.3-sampled-", .today))
# # make_quick_plots(m[[4]], id = paste0("-poisson-", .today))
# # make_quick_plots(m[[5]], id = paste0("-nb2-f-0.6-forecast-", .today))
# # make_quick_plots(m[[6]], id = paste0("-nb2-test-offset-", .today))
#
# # devtools::install_github("seananderson/ggsidekick")
#
#
# # Theta plots -----------------------------------------------------------------
#
# source("make_quick_plots.R")
# make_quick_plots(m[[1]], id = paste0("-nb2-", .today), ext = ".png")
#
# R0 <- purrr::map_df(m[1], function(.x) {
#   data.frame(theta = "R0", value = .x$post$R0, stringsAsFactors = FALSE)
# }, .id = "Scenario")
# phi <- purrr::map_df(m[1], function(.x) {
#   if ("phi" %in% names(.x$post)) {
#     data.frame(theta = "phi", value = .x$post$phi[,1], stringsAsFactors = FALSE)
#   } else {
#     data.frame(theta = "phi", value = NA, stringsAsFactors = FALSE)
#   }
# }, .id = "Scenario")
# f2 <- purrr::map_df(m[1], function(.x) {
#   data.frame(theta = "f2", value = .x$post$f2, stringsAsFactors = FALSE)
# }, .id = "Scenario")
# theta_df <- bind_rows(R0, f2) %>% as_tibble()
#
# # R0_prior <-
# ggplot(theta_df, aes(value)) +
#   facet_grid(Scenario~theta, scales = "free") +
#   geom_histogram(bins = 30, fill = "white", colour = "grey20") +
#   coord_cartesian(expand = FALSE, ylim = c(0, NA)) +
#   ylab("")
# ggsave(paste0("figs/theta-posteriors", .today, ".png"),
#   width = 5, height = 7)
