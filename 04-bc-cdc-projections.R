# Fits for BC CDC

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(ggplot2)
library(dplyr)
theme_set(ggsidekick::theme_sleek()) # devtools::install_github("seananderson/ggsidekick")

setwd(here::here("selfIsolationModel", "stan"))
source("fit_seeiqr.R")
source("make_projection_plot.R")
dat <- readr::read_csv(here::here("nCoVDailyData/CaseCounts/BC Case counts.csv"))
names(dat)[names(dat) == "BC"] <- "Cases"
dat$Date[71] <- "1/4/2020" # argh
dat$Date[72] <- "2/4/2020" # argh
dat$Date <- lubridate::dmy(dat$Date)
dat$day <- seq_len(nrow(dat))
dat$daily_diffs <- c(dat$Cases[2] - dat$Cases[1], diff(dat$Cases))
dat <- dplyr::filter(dat, Date >= "2020-03-01")
daily_diffs <- dat$daily_diffs
seeiqr_model <- stan_model("seeiqr.stan")
.today <- max(dat$Date)

# Across f2 = 0, 0.1, ... 1.0:
sd_strength <- seq(0, 1, 0.2) %>% purrr::set_names()
m_bccdc <- purrr::map(sd_strength, ~ {
  fit_seeiqr(
    daily_diffs,
    sampled_fraction1 = 0.1,
    sampled_fraction2 = 0.3,
    f2_prior = c(0.4, 0.2),
    R0_prior = c(log(2.6), 0.2),
    sampFrac2_type = "fixed",
    fixed_f_forecast = .x,
    delayScale = 11,
    seeiqr_model = seeiqr_model, chains = 6, iter = 600
  )
})

# With estimated f2 + 2 more days:
m_bccdc_est <- fit_seeiqr(
  c(daily_diffs, 34, 40),
  sampled_fraction1 = 0.1,
  sampled_fraction2 = 0.3,
  f2_prior = c(0.4, 0.2),
  R0_prior = c(log(2.6), 0.2),
  sampFrac2_type = "fixed",
  fixed_f_forecast = NULL,
  delayScale = 12,
  seeiqr_model = seeiqr_model, chains = 6, iter = 600
)

# saveRDS(m_bccdc, file = "models2/m_bccdc.rds")
# saveRDS(m_bccdc_est, file = "models2/m_bccdc_est.rds")
# m_bccdc <- readRDS("models2/m_bccdc.rds")

sd_est <- sprintf("%.2f", round(quantile(m_bccdc_est$post$f2, c(0.05, 0.5, 0.95)), 2))
sd_text <- paste0("(", sd_est[[2]], "; 90% CI: ", sd_est[1], "-", sd_est[3], ")")
sd_text

# Make combined plots ---------------------------------------------------------

# Experimental:

ylim <- c(0, 130)
ylim_c <- c(0, 3400)
names(m_bccdc) <- paste0("Contact fraction: ", sprintf("%.1f", sd_strength))
cols <- rep(RColorBrewer::brewer.pal(5, "Blues")[4], 6)
make_projection_plot(list("Plus 2 days from globalnews.ca" = m_bccdc_est), ylim = ylim, facet = TRUE, ncol = 2, cols = cols)
make_projection_plot(m_bccdc, ylim = ylim, facet = TRUE, ncol = 2, cols = cols)
make_projection_plot(m_bccdc, ylim = ylim_c, facet = TRUE, ncol = 2, cols = cols,
  cumulative = TRUE)

# Stop here.

# -----------------------------------------------------------------------------

# Previous:

make_projection_plot(m_bccdc, ylim = ylim, facet = FALSE)
ggsave(paste0("figs/case-projections-one-panel-", .today, ".png"),
  width = 8, height = 4.5)

make_projection_plot(m_bccdc, ylim = ylim_c, facet = FALSE, cumulative = TRUE)
ggsave(paste0("figs/cumulative-projections-one-panel-", .today, ".png"),
  width = 8, height = 4.5)

make_projection_plot(m_bccdc, ylim = ylim, facet = TRUE, ncol = 2)
ggsave(paste0("figs/case-projections-60days-", .today, ".png"),
  width = 5.5, height = 5.25)
make_projection_plot(m_bccdc, cumulative = TRUE, ylim = ylim_c, ncol = 2)
ggsave(paste0("figs/cumulative-case-projections-60days-", .today, ".png"),
  width = 5.5, height = 5.25)

make_projection_plot(m_bccdc, ylim = ylim, facet = TRUE, ncol = 2) +
  xlim(lubridate::ymd("2020-03-01"), lubridate::ymd("2020-05-08")) +
  theme(panel.spacing.x = unit(1, "lines"))
ggsave(paste0("figs/case-projections-30days-", .today, ".png"),
  width = 5.5, height = 5.25)
make_projection_plot(m_bccdc, cumulative = TRUE, ylim = ylim_c, ncol = 2) +
  xlim(lubridate::ymd("2020-03-01"), lubridate::ymd("2020-05-08"))
ggsave(paste0("figs/cumulative-case-projections-30days-", .today, ".png"),
  width = 5.5, height = 5.25)

# Split up into individual plots ----------------------------------------------

cols <- RColorBrewer::brewer.pal(8, "Dark2")[seq_along(m_bccdc)]
purrr::walk(seq_along(m_bccdc), function(i) {
  make_projection_plot(m_bccdc[i], cumulative = TRUE, ylim = ylim_c, ncol = 1, cols = cols[i]) +
    xlim(lubridate::ymd("2020-03-01"), lubridate::ymd("2020-05-08"))
  ggsave(paste0("figs/cumulative-case-projections-30days-", i, "-", .today, ".png"),
    width = 3.9, height = 3.25)
})

purrr::walk(seq_along(m_bccdc), function(i) {
  make_projection_plot(m_bccdc[i], cumulative = FALSE, ylim = ylim, ncol = 1, cols = cols[i]) +
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
        q0.05 = round(quantile(value, probs = 0.05)),
        q0.25 = round(quantile(value, probs = 0.25)),
        q0.5 = round(quantile(value, probs = 0.5)),
        q0.75 = round(quantile(value, probs = 0.75)),
        q0.95 = round(quantile(value, probs = 0.95)),
        mean = sprintf("%.1f", round(mean(value), 1))
      ) %>%
      mutate(forecast = day > obj$last_day_obs) %>%
      mutate(day = actual_dates[day]) %>%
      dplyr::filter(day <= lubridate::ymd("2020-06-08"))
  }, .id = "Scenario")
  out
}

get_dat_output(m_bccdc) %>%
  readr::write_csv(paste0("figs/case-projections-60-", .today, ".csv"))
get_dat_output(m_bccdc, cumulative = TRUE) %>%
  readr::write_csv(paste0("figs/cumulative-case-projections-60-", .today, ".csv"))

# -----------------------------------------------------------------------------
# What is the delay between the peak prevalence (I+Id) and the peak in case counts?

x <- seq(0, 25, length.out = 200);plot(x, dweibull(x, shape = 2, scale = 11), type = "l")
x <- seq(0, 25, length.out = 200);plot(x, dweibull(x, shape = 2, scale = 11), type = "l")
m_peak <- fit_seeiqr(
  daily_diffs, sampled_fraction1 = 0.1, sampled_fraction2 = 0.3,
  seeiqr_model = seeiqr_model, chains = 8, iter = 600,
  delayScale = 11,
  save_state_predictions = TRUE)
obj <- m_peak
post <- obj$post
variables_df <- dplyr::tibble(
  variable = names(obj$state_0),
  variable_num = seq_along(obj$state_0)
)
ts_df <- dplyr::tibble(time = obj$time, time_num = seq_along(obj$time))
states <- reshape2::melt(post$y_hat) %>%
  dplyr::rename(time_num = Var2, variable_num = Var3) %>%
  dplyr::left_join(variables_df, by = "variable_num") %>%
  dplyr::left_join(ts_df, by = "time_num") %>%
  as_tibble() %>%
  mutate(day = floor(time)) %>%
  dplyr::filter(variable %in% c("I", "Id")) %>%
  group_by(iterations, time) %>%
  summarize(I = value[variable == "I"], Id = value[variable == "Id"],
    prevalence = I + Id) %>%
  group_by(iterations) %>%
  dplyr::summarise(prevalence_peak = time[prevalence == max(prevalence)])

lambdas <- obj$post$lambda_d %>%
  reshape2::melt() %>%
  dplyr::rename(day = Var2) %>% as_tibble() %>%
  rename(case_count = value) %>%
  group_by(iterations) %>%
  dplyr::summarise(case_peak = day[case_count == max(case_count)])

both <- left_join(states, lambdas) %>%
  filter(case_peak != prevalence_peak) # one weird draw?
ggplot(tibble(delay = both$case_peak - both$prevalence_peak), aes(delay)) +
  geom_histogram(bins = 20)
ggsave("figs/delay-peak-prevalence.png", width = 5, height = 3.5)

states_timing <- states %>% mutate(start_decline = obj$stan_data$x_r[['start_decline']],
  end_decline = obj$stan_data$x_r[['end_decline']])
ggplot(states_timing, aes(prevalence_peak - start_decline)) + geom_histogram()
ggplot(states_timing, aes(prevalence_peak - end_decline)) + geom_histogram()
