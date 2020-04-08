library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
setwd(here::here("selfIsolationModel", "stan"))
source("fit_seeiqr.R")
dir.create("models2", showWarnings = FALSE)

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

ignore <- lapply(seq_along(m), function(x) {
  saveRDS(m[[x]], file = paste0("models2/stan-fit-", x, ".rds"))
})

# or start here to avoid re-fitting:
f <- list.files("models2", full.names = TRUE)
m <- lapply(seq_along(f), function(x) {
  readRDS(paste0("models2/stan-fit-", x, ".rds"))
})
names(m) <- c(
  "01. Strength of S.D.\nas estimated",
  "02. Strength of S.D.\nprojected 1.0",
  "03. Sampled 0.3;\nStrength of S.D.\nas estimated",
  "04. Poisson\nobservation error",
  "05. Strength of S.D.\nprojected 0.6",
  "06. Strength of S.D.\nprojected 0.2"
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
  first_date = "2020-03-01", ylim = c(0, 200), outer_quantile = c(0.025, 0.975),
  facet = TRUE, ncol = 1) {

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
  g <- ggplot(out, aes(x = day, y = med, ymin = lwr, ymax = upr, colour = Scenario,
    fill = Scenario)) +
    geom_ribbon(alpha = 0.2, colour = NA) +
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
    xlim(lubridate::ymd("2020-03-01"), lubridate::ymd("2020-05-08")) +
    geom_vline(xintercept = actual_dates[obj$last_day_obs], lty = 2, alpha = 0.6) +
    coord_cartesian(expand = FALSE, ylim = ylim) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols)

  if (facet)
    g <- g + facet_wrap(~Scenario, ncol = ncol) + theme(legend.position = "none")
   g
}

nm <- list()
nm[[1]] <- m[[1]]
# nm[[2]] <- m[[6]]
nm[[2]] <- m[[5]]
nm[[3]] <- m[[2]]
names(nm) <- names(m)[c(1, 5, 2)]

names(nm) <- c("1. Strength of S.D. estimated", "2. Strength of S.D. = 0.6 in future", "3. Strength of S.D. = 1.0 in future")

.today <- lubridate::today()
make_projection_plot(nm, ylim = c(0, 180), facet = FALSE)
ggsave(paste0("figs/case-projections-one-panel-", .today, ".png"),
  width = 8, height = 4.5)

make_projection_plot(mm, ylim = c(0, 3200), facet = FALSE, cumulative = TRUE)
ggsave(paste0("figs/cumulative-projections-one-panel-", .today, ".png"),
  width = 8, height = 4.5)

make_projection_plot(nm, ylim = c(0, 180), facet = TRUE)
ggsave(paste0("figs/case-projections-", .today, ".png"),
  width = 5.5, height = 7.25)
make_projection_plot(nm, cumulative = TRUE, ylim = c(0, 3200))
ggsave(paste0("figs/cumulative-case-projections-", .today, ".png"),
  width = 5.5, height = 7.25)

make_projection_plot(m, ylim = c(0, 180), facet = TRUE)
ggsave(paste0("figs/case-projections2-", .today, ".png"),
  width = 5.5, height = 10.25)
make_projection_plot(m, cumulative = TRUE, ylim = c(0, 3200))
ggsave(paste0("figs/cumulative-case-projections2-", .today, ".png"),
  width = 5.5, height = 10.25)


# Theta plots ---------------------------------------------------------

R0 <- purrr::map_df(m, function(.x) {
  data.frame(theta = "R0", value = .x$post$R0, stringsAsFactors = FALSE)
}, .id = "Scenario")
phi <- purrr::map_df(m, function(.x) {
  if ("phi" %in% names(.x$post)) {
    data.frame(theta = "phi", value = .x$post$phi[,1], stringsAsFactors = FALSE)
  } else {
    data.frame(theta = "phi", value = NA, stringsAsFactors = FALSE)
  }
}, .id = "Scenario")
f2 <- purrr::map_df(m, function(.x) {
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
  width = 5, height = 12)


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
