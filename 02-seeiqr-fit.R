library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
setwd(here::here("selfIsolationModel", "stan"))
source("fit_seeiqr.R")
dir.create("models", showWarnings = FALSE)

dat <- readr::read_csv(here::here("nCoVDailyData/CaseCounts/BC Case counts.csv"))
names(dat)[names(dat) == "BC"] <- "Cases"
dat$Date[71] <- "1/4/2020" # argh
dat$Date[72] <- "2/4/2020" # argh
dat$Date <- lubridate::dmy(dat$Date)
dat$day <- seq_len(nrow(dat))
dat$daily_diffs <- c(
  dat$Cases[2] - dat$Cases[1],
  diff(dat$Cases)
)
# FIXME: Fudge this for now to give same start date as `bcdata`,
# which the initial conditions have been tuned to.
dat <- dplyr::filter(dat, Date >= "2020-03-01")
daily_diffs <- dat$daily_diffs
plot(daily_diffs)

seeiqr_model <- stan_model("seeiqr.stan")

# in progress!
m <- list()
m[[1]] <- fit_seeiqr(
  daily_diffs,
  seeiqr_model = seeiqr_model)
m[[2]] <- fit_seeiqr(
  daily_diffs,
  fixed_f_forecast = 1.0,
  seeiqr_model = seeiqr_model)
m[[3]] <- fit_seeiqr(
  daily_diffs,
  sampled_fraction1 = 0.3,
  sampled_fraction2 = 0.3,
  seeiqr_model = seeiqr_model)
m[[4]] <- fit_seeiqr(
  daily_diffs,
  obs_model = "Poisson",
  seeiqr_model = seeiqr_model)
m[[5]] <- fit_seeiqr(
  daily_diffs,
  fixed_f_forecast = 0.6,
  seeiqr_model = seeiqr_model)
m[[6]] <- fit_seeiqr(
  daily_diffs,
  fixed_f_forecast = 0.2,
  seeiqr_model = seeiqr_model)
m[[7]] <- fit_seeiqr(
  daily_diffs,
  sampled_fraction1 = 0.2,
  sampled_fraction2 = 0.7,
  sampled_fraction_day_change = 12,
  seeiqr_model = seeiqr_model)
m[[8]] <- fit_seeiqr(
  daily_diffs,
  sampled_fraction1 = 0.2,
  sampled_fraction2 = 0.7,
  sampled_fraction_day_change = 12,
  pars = c(
    N = 4.4e6, D = 4, k1 = 1 / 5,
    k2 = 1, q = 0.05,
    r = 1, ur = 0.2, f1 = 1.0,
    start_decline = 15,
    end_decline = 22
  ),
  seeiqr_model = seeiqr_model)
m[[9]] <- fit_seeiqr(
  daily_diffs,
  sampled_fraction1 = 0.2,
  sampled_fraction2 = 0.7,
  sampled_fraction_day_change = 12,
  pars = c(
    N = 4.4e6, D = 4, k1 = 1 / 4,
    k2 = 1, q = 0.05,
    r = 1, ur = 0.2, f1 = 1.0,
    start_decline = 15,
    end_decline = 22
  ),
  seeiqr_model = seeiqr_model)
m[[10]] <- fit_seeiqr(
  daily_diffs,
  sampled_fraction1 = 0.2,
  sampled_fraction2 = 0.7,
  sampled_fraction_day_change = 12,
  pars = c(
    N = 4.4e6, D = 4, k1 = 1 / 4,
    k2 = 1, q = 0.05,
    r = 1, ur = 0.2, f1 = 1.0,
    start_decline = 15,
    end_decline = 22
  ),
  delayScale = 9,
  seeiqr_model = seeiqr_model)
m[[11]] <- fit_seeiqr(
  daily_diffs,
  sampled_fraction1 = 0.2,
  sampled_fraction2 = 0.7,
  sampled_fraction_day_change = 12,
  delayScale = 9,
  seeiqr_model = seeiqr_model)
m[[12]] <- fit_seeiqr(
  daily_diffs,
  delayScale = 9,
  seeiqr_model = seeiqr_model)
m[[13]] <- fit_seeiqr(
  daily_diffs,
  sampled_fraction1 = 0.1,
  sampled_fraction2 = 0.7,
  sampled_fraction_day_change = 12,
  seeiqr_model = seeiqr_model)

# ignore <- lapply(seq_along(m), function(x) {
#   saveRDS(m[[x]], file = paste0("models/stan-fit-", x, ".rds"))
# })
#
# # or start here to avoid re-fitting:
# f <- list.files("models", full.names = TRUE)
# m <- lapply(seq_along(f), function(x) {
#   readRDS(paste0("models/stan-fit-", x, ".rds"))
# })
names(m) <- c(
  "01. Strength of S.D.\nas estimated",
  "02. Strength of S.D.\nprojected 1.0",
  "03. Sampled 0.3;\nStrength of S.D.\nas estimated",
  "04. Poisson",
  "05. Strength of S.D.\nprojected 0.6",
  "06. Strength of S.D.\nprojected 0.2",
  "07. Sampling 0.2-0.7\non day 12",
  "08. ur = 0.2; D = 4",
  "09. ur = 0.2; D = 4; k = 1/4",
  "10. ur = 0.2; D = 4; k = 1/4;\ndelayScale = 9",
  "11. Sampling 0.2-0.7\non day 12;\ndelayScale = 9",
  "12. Strength of S.D.\nas estimated\ndelayScale = 9",
  "13. Sampling 0.1-0.7\non day 12"
  )

# Check summary:
# e.g.
# print(m[[1]]$fit, pars = c("R0", "f2", "phi"))
# print(m[[2]]$fit, pars = c("R0", "f2", "phi"))
# print(m[[3]]$fit, pars = c("R0", "f2", "phi"))

library(dplyr)
library(ggplot2)
# devtools::install_github("seananderson/ggsidekick")
theme_set(ggsidekick::theme_sleek())

make_projection_plot <- function(models, cumulative = FALSE,
  first_date = "2020-03-01", ylim = c(0, 200), outer_quantile = c(0.025, 0.975)) {

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
        lwr = quantile(value, probs = outer_quantile[1]),
        lwr2 = quantile(value, probs = 0.25),
        upr = quantile(value, probs = outer_quantile[2]),
        upr2 = quantile(value, probs = 0.75),
        med = median(value)
      ) %>%
      mutate(day = actual_dates[day])
  }, .id = "Scenario")

  if (cumulative) {
    dat <- tibble(day = actual_dates[1:obj$last_day_obs],
      value = cumsum(obj$daily_cases))
  } else {
    dat <- tibble(day = actual_dates[1:obj$last_day_obs],
      value = obj$daily_cases)
  }
  cols <- RColorBrewer::brewer.pal(8, "Dark2")
  cols <- rep(cols, 5)
  ggplot(out, aes(x = day, y = med, ymin = lwr, ymax = upr, colour = Scenario,
    fill = Scenario)) +
    geom_ribbon(alpha = 0.2, colour = NA) +
    facet_wrap(~Scenario, ncol = 3) +
    geom_ribbon(alpha = 0.2, mapping = aes(ymin = lwr2, ymax = upr2), colour = NA) +
    geom_line(alpha = 0.9, lwd = 1) +
    geom_point(
      data = dat,
      col = "black", inherit.aes = FALSE, aes(x = day, y = value),
    ) +
    geom_line(
      data = dat,
      col = "black", inherit.aes = FALSE, aes(x = day, y = value), lwd = 0.3,
      alpha = 0.8
    ) +
    ylab(if (!cumulative) "New cases" else "Cumulative cases") +
    xlab("Day") +
    geom_vline(xintercept = actual_dates[obj$last_day_obs], lty = 2, alpha = 0.6) +
    coord_cartesian(expand = FALSE, ylim = ylim) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    theme(legend.position = "none")
}

.today <- lubridate::today()
make_projection_plot(m)
ggsave(paste0("figs/case-projections-", .today, ".png"),
  width = 8, height = 10)
# make_projection_plot(m, cumulative = TRUE, ylim = c(0, 3500))
# ggsave(paste0("figs/cumulative-case-projections-", .today, ".png"),
#   width = 6, height = 9)


# Theta plots ---------------------------------------------------------

m2 <- m

R0 <- purrr::map_df(m2, function(.x) {
  data.frame(theta = "R0", value = .x$post$R0, stringsAsFactors = FALSE)
}, .id = "Scenario")
phi <- purrr::map_df(m2, function(.x) {
  if ("phi" %in% names(.x$post)) {
    data.frame(theta = "phi", value = .x$post$phi[,1], stringsAsFactors = FALSE)
  } else {
    data.frame(theta = "phi", value = NA, stringsAsFactors = FALSE)
  }
}, .id = "Scenario")
f2 <- purrr::map_df(m2, function(.x) {
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
  width = 5, height = 18)


# --------------------------------------------------

## Andy's code, to be integrated...
# Load in number of tests each day:
# Crude for now - want to check how the numbers of cases (positive tests)
# compare with the ones in dat. Could scale them up perhaps.
# load(paste0(here::here(),
#   "/nCoVDailyData/Labdata/testsanonym.RData"))
# # Only contains dataframe 'testsanonymized'
# tests_anon <- dplyr::as_tibble(testsanonymized) %>%
#   type.convert() %>%
#   dplyr::mutate(results_date = lubridate::date(results_date))
# tests_by_day <- tests_anon %>%
#   dplyr::group_by(results_date) %>%
#   dplyr::count(name = "total_tests")
# total_tests <- dplyr::filter(tests_by_day,
#   results_date %in% unique(dat$Date))$total_tests
# length(daily_diffs)
# length(total_tests)
# diff(dat$Date)
# plot(total_tests)
# plot(daily_diffs/total_tests)
# total_tests <- c(total_tests, rep(total_tests[length(total_tests)], 60)) # 60 day forecast
#
# m[[6]] <- fit_seeiqr(
#   daily_diffs,
#   fixed_f_forecast = 0.6,
#   forecast_days = 60,
#   daily_tests = total_tests,
#   seeiqr_model = seeiqr_model)

## Andy's code, to be integrated...
# TODO: setup-dates.R explains how Andy is setting up the dates (it's mostly
# explanations that I didn't want to clutter up here).
# Load in the detailed case data of estimated times between people's onset of
# symptoms and their test becoming a 'reported case'. Create delay_data tibble(),
# where for the negative binomial model you want the time_to_report column (may
# need as.numeric() as they are in days).
# source(here::here("/selfIsolationModel/SIR-functionalised/funcs.R"))
# # Just need the one function:
# delay_data <- load_tidy_delay_data()[["delay_data"]]
