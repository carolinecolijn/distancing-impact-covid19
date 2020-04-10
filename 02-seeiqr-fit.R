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
dat <- dplyr::filter(dat, Date >= "2020-03-01")
daily_diffs <- dat$daily_diffs
plot(daily_diffs)

seeiqr_model <- stan_model("seeiqr.stan")

# Look at sample fraction scenarios -------------------------------------------

library(dplyr)
sf1 <- tidyr::expand_grid(sampled_fraction1 = c(0.05), sampled_fraction2 = c(0.1, 0.2, 0.3))
sf2 <- tidyr::expand_grid(sampled_fraction1 = c(0.1), sampled_fraction2 = c(0.2, 0.3, 0.4))
sf <- bind_rows(sf1, sf2)

library(future)
plan(multisession, workers = parallel::detectCores()/2)
m_sf <- furrr::future_pmap(sf, function(sampled_fraction1, sampled_fraction2) {
  fit_seeiqr(
    daily_diffs, chains = 1, iter = 400,
    sampled_fraction1 = sampled_fraction1, sampled_fraction2 = sampled_fraction2,
    seeiqr_model = seeiqr_model)
})
purrr::walk(m_sf, ~ print(.x$fit, pars = c("R0", "f2", "phi", "sampFrac2")))
plan(sequential)
names(m_sf) <- paste0("sampFrac1 = ", sf$sampled_fraction1, "\nsampFrac2 = ", sf$sampled_fraction2, "\n")
theme_set(ggsidekick::theme_sleek())
source("make_projection_plot.R")
make_projection_plot(m_sf, ylim = c(0, 100), facet = FALSE, outer_quantile = c(0.25, 0.75))
ggsave(paste0("figs/sampFrac-grid.png"), width = 7, height = 3.5)

R0 <- purrr::map_df(m_sf, function(.x) {
  data.frame(theta = "R0", value = .x$post$R0, stringsAsFactors = FALSE)
}, .id = "Scenario")
f2 <- purrr::map_df(m_sf, function(.x) {
  data.frame(theta = "f2", value = .x$post$f2, stringsAsFactors = FALSE)
}, .id = "Scenario")
theta_df <- bind_rows(R0, f2) %>% as_tibble()
my_limits <- function(x) if (max(x) < 2) c(0, 1) else c(2, 3)
ggplot(theta_df, aes(value)) +
  facet_grid(Scenario~theta, scales = "free") +
  geom_histogram(bins = 50, fill = "white", colour = "grey20") +
  coord_cartesian(expand = FALSE, ylim = c(0, NA)) +
  ylab("") +
  scale_x_continuous(limits = my_limits) + xlab("Parameter value") + ylab("Density")
ggsave(paste0("figs/sampFrac-grid-theta-posteriors.png"),
  width = 5, height = 7)

# sampFrac random walk --------------------------------------------------------

m_rw <- fit_seeiqr(
  daily_diffs,
  sampled_fraction1 = 0.1,
  sampFrac2_type = "rw",
  rw_sigma = 0.1,
  sampFrac2_prior = c(0.4, 0.2),
  fixed_f_forecast = 1.0,
  seeiqr_model = seeiqr_model, chains = 6, cores = 400)
m_rw$post$sampFrac2 %>% reshape2::melt() %>% as_tibble() %>%
  rename(day = Var2) %>%
  group_by(day) %>%
  summarise(
    lwr = quantile(value, probs = 0.025),
    lwr2 = quantile(value, probs = 0.25),
    upr = quantile(value, probs = 0.975),
    upr2 = quantile(value, probs = 0.75),
    med = median(value)
  ) %>%
  ggplot(aes(day, med)) + geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_ribbon(aes(ymin = lwr2, ymax = upr2), alpha = 0.9) +
  ylab("Sampled fraction") + xlab("Days after March 14")
ggsave(paste0("figs/sampFrac2-rw.png"), width = 5, height = 4)

# -----------------------------------------------------------------------------

length(daily_diffs)
sampled_fraction_vec <- c(
  rep(0.2, 14),
  approx(c(15, 20), c(0.2, 0.5), xout = seq(15, 20))$y,
  approx(c(21, 40), c(0.5, 0.3), xout = seq(21, 39))$y,
  rep(0.3, 60)
)
plot(sampled_fraction_vec)

m4 <- fit_seeiqr(
  daily_diffs, chains = 6, iter = 300,
  seeiqr_model = seeiqr_model, sampled_fraction_vec = sampled_fraction_vec,
  seeiqr_model = seeiqr_model)

library(future)
fs <- seq(0.3, 1, 0.1)
plan(multisession, workers = parallel::detectCores()/2)
m <- furrr::future_map(fs, function(.f) {
  fit_seeiqr(
    daily_diffs, iter = 300, chains = 1, save_state_predictions = TRUE,
      seeiqr_model = seeiqr_model, fixed_f_forecast = .f)
})
plan(future::sequential)

purrr::walk(m, ~ print(.x$fit, pars = c("R0", "f2", "phi", "sampFrac2")))

# Identify the point at which prevalence slope goes to 0: ---------------------

library(dplyr)
library(ggplot2)

get_prevalence_slope <- function(obj, f_val) {
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
    as_tibble()
  temp <- states %>%
    dplyr::filter(time > max(states$time) - 30, variable %in% c("I", "Id")) %>%
    group_by(iterations, time) %>%
    summarize(I = value[variable == "I"], Id = value[variable == "Id"],
      prevalence = I + Id)
  iters <- temp %>% group_by(iterations) %>% summarise(iter = iterations[[1]])
  temp %>%
    group_by(iterations) %>%
    group_split() %>%
    purrr::map(~lm(log(prevalence) ~ time, data = .x)) %>%
    purrr::map_df(~tibble(slope = coef(.x)[[2]])) %>%
    mutate(f = f_val) %>%
    ungroup() %>%
    mutate(iterations = iters$iter)
}
slopes <- purrr::map2_df(m, fs, get_prevalence_slope)
theme_set(ggsidekick::theme_sleek())
ggplot(slopes, aes(f, slope)) +
  geom_jitter(height = 0, alpha = 0.1, width = 0.2)

mlm <- lm(slope ~ f, data = slopes)
nd <- data.frame(f = seq(0.3, 1, length.out = 2000))
nd$predicted_slope <- predict(mlm, newdata = nd)
thresh <- dplyr::filter(nd, predicted_slope > 0) %>% `[`(1, 'f')
ggplot(slopes, aes(f, slope)) +
  geom_point(alpha = 0.04) +
  geom_line(data = nd, aes(f, predicted_slope), alpha = 0.3) +
  geom_vline(xintercept = thresh, lty = 2, alpha = 0.6) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.6) + ylab("Slope of log(prevalence) vs. day") +
  xlab("Social distancing strength")
ggsave("figs/f-threshold.png", width = 3.7, height = 3.5)

# Make joint posterior plot with prevalence colouring: ------------------------

m_yhat <- fit_seeiqr(
  daily_diffs, iter = 300, chains = 8, save_state_predictions = TRUE,
  seeiqr_model = seeiqr_model)

joint_post <- tibble(R0 = m_yhat$post$R0, f2 = m_yhat$post$f2, iterations = seq_along(f2))
prev_slopes <- get_prevalence_slope(m_yhat, "estimated") %>%
  mutate(perc_change = 100 * (exp(slope) - 1))
joint_post2 <- left_join(joint_post, prev_slopes)
ggplot(joint_post2, aes(R0, f2, colour = -perc_change)) +
  geom_point(alpha = 0.15, size = 2) +
  geom_point(alpha = 0.5, size = 2, pch = 21) +
  scale_colour_viridis_c(option = "D", direction = -1) +
  labs(colour = "Percent decline\nper day", y = "Social distancing strength",
    x = expression(R[0])) +
  theme(legend.position = c(0.81, 0.78)) +
  theme(legend.key.size = unit(11, units = "points"))
ggsave("figs/joint-posterior-prevalence.png", width = 3.7, height = 3.5)

# -----------------------------------------------------------------------------

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
