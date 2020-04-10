# start by sourcing `02-seeiqr-fit.R`

library(ggplot2)
library(dplyr)
theme_set(theme_light())
source("make_quick_plots.R")
setwd(here::here("selfIsolationModel", "stan"))
dir.create("figs", showWarnings = FALSE)
.today <- lubridate::today()

# make_quick_plots(m[[1]], id = paste0("-nb2-", .today))
# make_quick_plots(m[[2]], id = paste0("-nb2-f-1.0-forecast-", .today))
# make_quick_plots(m[[3]], id = paste0("-nb2-0.3-sampled-", .today))
# make_quick_plots(m[[4]], id = paste0("-poisson-", .today))
# make_quick_plots(m[[5]], id = paste0("-nb2-f-0.6-forecast-", .today))
# make_quick_plots(m[[6]], id = paste0("-nb2-test-offset-", .today))

# devtools::install_github("seananderson/ggsidekick")
theme_set(ggsidekick::theme_sleek())
source("make_projection_plot.R")
.today <- max(dat$Date)

make_projection_plot(m, ylim = c(0, 180), facet = FALSE)
ggsave(paste0("figs/case-projections-one-panel-", .today, ".png"),
  width = 8, height = 4.5)

make_projection_plot(m, ylim = c(0, 3200), facet = FALSE, cumulative = TRUE)
ggsave(paste0("figs/cumulative-projections-one-panel-", .today, ".png"),
  width = 8, height = 4.5)

make_projection_plot(m, ylim = c(0, 180), facet = TRUE, ncol = 2)
ggsave(paste0("figs/case-projections-60days-", .today, ".png"),
  width = 5.5, height = 5.25)
make_projection_plot(m, cumulative = TRUE, ylim = c(0, 3200), ncol = 2)
ggsave(paste0("figs/cumulative-case-projections-60days", .today, ".png"),
  width = 5.5, height = 5.25)

# cols <- viridisLite::viridis(4, end =  0.92)
make_projection_plot(m, ylim = c(0, 180), facet = TRUE, ncol = 2) +
  xlim(lubridate::ymd("2020-03-01"), lubridate::ymd("2020-05-08")) +
  theme(panel.spacing.x = unit(1, "lines"))
ggsave(paste0("figs/case-projections-30days", .today, ".png"),
  width = 5.5, height = 5.25)
make_projection_plot(m, cumulative = TRUE, ylim = c(0, 3200), ncol = 2) +
  xlim(lubridate::ymd("2020-03-01"), lubridate::ymd("2020-05-08"))
ggsave(paste0("figs/cumulative-case-projections-30days-", .today, ".png"),
  width = 5.5, height = 5.25)

# Split up into individual plots ----------------------------------------------

cols <- RColorBrewer::brewer.pal(8, "Dark2")[seq_along(m)]
# cols <- viridisLite::viridis(4, end =  0.92)
purrr::walk(seq_along(m), function(i) {
  make_projection_plot(m[i], cumulative = TRUE, ylim = c(0, 3200), ncol = 1, cols = cols[i]) +
    xlim(lubridate::ymd("2020-03-01"), lubridate::ymd("2020-05-08"))
  ggsave(paste0("figs/cumulative-case-projections-30days-", i, "-", .today, ".png"),
    width = 3.9, height = 3.25)
})

purrr::walk(seq_along(m), function(i) {
  make_projection_plot(m[i], cumulative = FALSE, ylim = c(0, 180), ncol = 1, cols = cols[i]) +
    xlim(lubridate::ymd("2020-03-01"), lubridate::ymd("2020-05-08"))
  ggsave(paste0("figs/case-projections-30days-", i, "-", .today, ".png"),
    width = 3.9, height = 3.25)
})

# CSV output ------------------------------------------------------------------

get_dat_output <- function(models, cumulative = FALSE, first_date = "2020-03-01") {
  obj <- models[[1]]
  actual_dates <- seq(lubridate::ymd(first_date),
    lubridate::ymd(first_date) + max(obj$days), by = "1 day")
  out <- purrr::map_df(models, function(.x) {
    temp <- .x$post$y_rep %>%
      reshape2::melt() %>%
      dplyr::rename(day = Var2)
    if (cumulative) {
      temp <- temp %>%
        group_by(iterations) %>%
        mutate(value = cumsum(value)) %>%
        ungroup()
    }
    temp %>%
      group_by(day) %>%
      summarise(
        q0.025 = round(quantile(value, probs = 0.025)),
        q0.05 = round(quantile(value, probs = 0.05)),
        q0.25 = round(quantile(value, probs = 0.25)),
        q0.5 = round(quantile(value, probs = 0.5)),
        q0.75 = round(quantile(value, probs = 0.75)),
        q0.95 = round(quantile(value, probs = 0.95)),
        q0.975 = round(quantile(value, probs = 0.975)),
        mean = sprintf("%.1f", round(mean(value), 1))
      ) %>%
      mutate(forecast = day > obj$last_day_obs) %>%
      mutate(day = actual_dates[day]) %>%
      dplyr::filter(day <= lubridate::ymd("2020-06-08"))
  }, .id = "Scenario")
  out
}

get_dat_output(m) %>%
  readr::write_csv(paste0("figs/case-projections-60-", .today, ".csv"))
get_dat_output(m, cumulative = TRUE) %>%
  readr::write_csv(paste0("figs/cumulative-case-projections-60-", .today, ".csv"))

# Theta plots -----------------------------------------------------------------

source("make_quick_plots.R")
make_quick_plots(m[[1]], id = paste0("-nb2-", .today), ext = ".png")

R0 <- purrr::map_df(m[1], function(.x) {
  data.frame(theta = "R0", value = .x$post$R0, stringsAsFactors = FALSE)
}, .id = "Scenario")
phi <- purrr::map_df(m[1], function(.x) {
  if ("phi" %in% names(.x$post)) {
    data.frame(theta = "phi", value = .x$post$phi[,1], stringsAsFactors = FALSE)
  } else {
    data.frame(theta = "phi", value = NA, stringsAsFactors = FALSE)
  }
}, .id = "Scenario")
f2 <- purrr::map_df(m[1], function(.x) {
  data.frame(theta = "f2", value = .x$post$f2, stringsAsFactors = FALSE)
}, .id = "Scenario")
theta_df <- bind_rows(R0, f2) %>% as_tibble()

# R0_prior <-
ggplot(theta_df, aes(value)) +
  facet_grid(Scenario~theta, scales = "free") +
  geom_histogram(bins = 30, fill = "white", colour = "grey20") +
  coord_cartesian(expand = FALSE, ylim = c(0, NA)) +
  ylab("")
ggsave(paste0("figs/theta-posteriors", .today, ".png"),
  width = 5, height = 7)
