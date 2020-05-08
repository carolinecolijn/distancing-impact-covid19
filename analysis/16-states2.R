library(ggplot2)
library(dplyr)
library(future)
library(covidseir)
plan(multisession)
options(mc.cores = parallel::detectCores() / 2)
ymd <- lubridate::ymd

d <- readr::read_csv("https://covidtracking.com/api/v1/states/daily.csv")
readr::write_csv(d, "data-generated/us-data.csv")
d <- readr::read_csv("data-generated/us-data.csv")
d$date <- lubridate::ymd(d$date)

g <- d %>% filter(date > lubridate::ymd("2020-03-01")) %>%
  ggplot(aes(date, positiveIncrease)) +
  geom_line() +
  facet_wrap(~state, scales = "free_y") +
  ggsidekick::theme_sleek() +
  theme(axis.text.x = element_text(angle = 90))
ggsave("figs-ms/states-positive-increase.png", width = 12, height = 8)

g <- d %>% filter(date > lubridate::ymd("2020-03-01")) %>%
  ggplot(aes(date, hospitalizedIncrease))
  geom_line() +
  facet_wrap(~state, scales = "free_y") +
  ggsidekick::theme_sleek() +
  theme(axis.text.x = element_text(angle = 90))
ggsave("figs-ms/states-hospitalized-increase.png", width = 12, height = 8)

g <- d %>% filter(date > lubridate::ymd("2020-03-01")) %>%
  ggplot(aes(date, totalTestResultsIncrease)) +
  geom_line() +
  facet_wrap(~state, scales = "free_y") +
  ggsidekick::theme_sleek() +
  theme(axis.text.x = element_text(angle = 90))
ggsave("figs-ms/states-tests-increase.png", width = 12, height = 8)

new_york <- filter(d, state %in% "NY") %>%
  select(date, positiveIncrease, totalTestResultsIncrease, hospitalizedIncrease) %>%
  filter(date >= ymd("2020-03-05")) %>%
  rename(value = positiveIncrease, tests = totalTestResultsIncrease, hospitalized = hospitalizedIncrease) %>%
  arrange(date) %>%
  mutate(day = seq_len(n()))

new_york
# View(new_york)

plot(new_york$day, new_york$value, type = "o")
plot(new_york$day, new_york$tests, type = "o")
plot(new_york$date, new_york$value, type = "l")
lines(new_york$date, new_york$hospitalized, col = "red")
lines(new_york$date, new_york$tests/10, col = "blue")

# Tests Jump on day 9 from <100 to >2000
# and to > 10,000 by the 16th

samp_frac_fixed <- rep(0.25, nrow(new_york))

# NM: first confirmed case was Mar 1st, large gatherings banned Mar 12th,
# increased SD measures until stay-at-home order issued on Mar 22nd.

# https://en.wikipedia.org/wiki/COVID-19_pandemic_in_New_York_(state)#Timeline
# On March 8, the state reported 16 new confirmed cases and a total of 106 cases
# statewide.[24] New York City issued new commuter guidelines amid the current
# outbreak, asking sick individuals to stay off public transit, encouraging
# citizens to avoid densely packed buses, subways, or trains.[25]

# On March 11, Cuomo announced that the City University of New York and State
# University of New York schools would be closed for the following week, from
# March 12 to 19.

# April 26, 2020: https://coronavirus.health.ny.gov/system/files/documents/2020/04/doh_covid19_revisedtestingprotocol_042620.pdf
# Diagnostic and/or serologic testing for COVID-19 shall be authorized by a health care provider when:
#   • An individual is symptomatic or has a history of symptoms of COVID-19 (e.g. fever, cough, and/or trouble breathing), particularly if the individual is 70 years of age or older, the individual has a compromised immune system, or the individual has an underlying health condition; or
# • An individual has had close (i.e. within six feet) or proximate contact with a person known to be positive with COVID-19; or
# • An individual is subject to a precautionary or mandatory quarantine; or
# • An individual is employed as a health care worker, first responder, or other essential
# worker who directly interacts with the public while working; or
# • An individual presents with a case where the facts and circumstances – as determined
# by the treating clinician in consultation with state or local department of health officials – warrant testing.

# https://www.medrxiv.org/content/10.1101/2020.04.20.20073338v1.full.pdf
# "As of April 15, New York still recommended restricting testing to those with a known positive contact or travel history, as well as symptomatic individuals who had tested negative for other infections"

get_days_since <- function(until, since) {
  abs(as.numeric(difftime(until, since, units = "days")))
}
(start_decline <- get_days_since(ymd("2020-03-12"), min(new_york$date)))
(end_decline <- get_days_since(ymd("2020-03-27"), min(new_york$date)))
(f_seg <- c(rep(0, start_decline), rep(1, nrow(new_york) - start_decline)))

get_days_since(ymd("2020-03-22"), min(new_york$date))
min(new_york$date) + start_decline
min(new_york$date) + end_decline

# BC: start_decline = 15, end_decline = 22

vec <- c(17, 19, 21, 23, 25)
m2 <- furrr::future_map(vec, function(x) {
  new_york_fit <- covidseir::fit_seir(
    daily_cases = new_york$value,
    samp_frac_fixed = samp_frac_fixed,
    time_increment = 0.2,
    f_seg = f_seg,
    R0_prior = c(log(4.7), 0.2),
    iter = 300,
    i0 = 6,
    seed = 123,
    # delay_scale = 7,
    # delay_shape = 1.73,
    pars = c(N = 19.45e6, D = 5, k1 = 1/5, k2 = 1,
      q = 0.05, r = 0.1, ur = 0.02, f0 = 1,
      start_decline = start_decline, end_decline = x),
    chains = 1)
  new_york_fit
})

g <- purrr::map2(m2, vec, function(x, y) {
  p <- covidseir::project_seir(x, iter = 1:50)
  covidseir::tidy_seir(p, resample_y_rep = 30) %>%
    covidseir::plot_projection(new_york) +
    ggtitle(y)
})

cowplot::plot_grid(plotlist = g)

# D ---------------------------------------------------------------------------

vec <- c(3, 4, 5, 6, 7)
m3 <- furrr::future_map(vec, function(x) {
  new_york_fit <- covidseir::fit_seir(
    daily_cases = new_york$value,
    samp_frac_fixed = samp_frac_fixed,
    time_increment = 0.2,
    f_seg = f_seg,
    R0_prior = c(log(4.7), 0.2),
    iter = 250,
    i0 = 6,
    seed = 123,
    # delay_scale = 7,
    # delay_shape = 1.73,
    pars = c(N = 19.45e6, D = x, k1 = 1/5, k2 = 1,
      q = 0.05, r = 0.1, ur = 0.02, f0 = 1,
      start_decline = start_decline, end_decline = 23),
    chains = 1)
  new_york_fit
})

g <- purrr::map2(m3, vec, function(x, y) {
  p <- covidseir::project_seir(x, iter = 1:50)
  covidseir::tidy_seir(p, resample_y_rep = 30) %>%
    covidseir::plot_projection(new_york) +
    ggtitle(y)
})

cowplot::plot_grid(plotlist = g)

# Starting date ---------------------------------------------------------------

get_days_since(ymd("2020-03-12"), min(new_york$date))

vec <- c(7, 8, 9, 10, 11, 12, 13, 14)
m4 <- furrr::future_map(vec, function(x) {
  f_seg <- c(rep(0, x), rep(1, nrow(new_york) - x))
  new_york_fit <- covidseir::fit_seir(
    daily_cases = new_york$value,
    samp_frac_fixed = samp_frac_fixed,
    time_increment = 0.2,
    f_seg = f_seg,
    R0_prior = c(log(4.7), 0.2),
    iter = 300,
    i0 = 6,
    seed = 123,
    # delay_scale = 7,
    # delay_shape = 1.73,
    pars = c(N = 19.45e6, D = x, k1 = 1/5, k2 = 1,
      q = 0.05, r = 0.1, ur = 0.02, f0 = 1,
      start_decline = x, end_decline = 25),
    chains = 1)
  new_york_fit
})

g <- purrr::map2(m4, vec, function(x, y) {
  p <- covidseir::project_seir(x, iter = 1:50)
  covidseir::tidy_seir(p, resample_y_rep = 30) %>%
    covidseir::plot_projection(new_york) +
    ggtitle(y)
})

cowplot::plot_grid(plotlist = g)

# i0 -------------------------------------

vec <- c(1, 2, 4, 8)
m4 <- furrr::future_map(vec, function(x) {
  f_seg <- c(rep(0, 11), rep(1, nrow(new_york) - 11))
  samp_frac_fixed <- rep(0.25, nrow(new_york))
  new_york_fit <- covidseir::fit_seir(
    daily_cases = new_york$value,
    samp_frac_fixed = samp_frac_fixed,
    time_increment = 0.2,
    f_seg = f_seg,
    R0_prior = c(log(4.7), 0.2),
    iter = 200,
    i0 = 6 * x,
    seed = 42,
    # delay_scale = 7,
    # delay_shape = 1.73,
    pars = c(N = 19.45e6, D = x, k1 = 1/5, k2 = 1,
      q = 0.05, r = 0.1, ur = 0.02, f0 = 1,
      start_decline = 11, end_decline = 25),
    chains = 1)
  new_york_fit
})

g <- purrr::map2(m4, vec, function(x, y) {
  p <- covidseir::project_seir(x, iter = 1:50)
  covidseir::tidy_seir(p, resample_y_rep = 30) %>%
    covidseir::plot_projection(new_york) +
    ggtitle(y)
})

cowplot::plot_grid(plotlist = g)


# sampFrac -------------------------------------

vec <- c(0.1, 0.2, 0.25, 0.3, 0.4)
m5 <- furrr::future_map(vec, function(x) {
  f_seg <- c(rep(0, 11), rep(1, nrow(new_york) - 11))
  samp_frac_fixed <- rep(x, nrow(new_york))
  new_york_fit <- covidseir::fit_seir(
    daily_cases = new_york$value,
    samp_frac_fixed = samp_frac_fixed,
    time_increment = 0.2,
    f_seg = f_seg,
    R0_prior = c(log(4.7), 0.2),
    iter = 200,
    i0 = 6 * 1,
    seed = 42,
    # delay_scale = 7,
    # delay_shape = 1.73,
    pars = c(N = 19.45e6, D = x, k1 = 1/5, k2 = 1,
      q = 0.05, r = 0.1, ur = 0.02, f0 = 1,
      start_decline = 11, end_decline = 25),
    chains = 1)
  new_york_fit
})

g <- purrr::map2(m5, vec, function(x, y) {
  p <- covidseir::project_seir(x, iter = 1:50)
  covidseir::tidy_seir(p, resample_y_rep = 30) %>%
    covidseir::plot_projection(new_york) +
    ggtitle(y)
})

cowplot::plot_grid(plotlist = g)

# ----------------------

f_seg <- c(rep(0, 11), rep(1, nrow(new_york) - 11))
samp_frac_fixed
ny_fit <- covidseir::fit_seir(
  daily_cases = new_york$value,
  samp_frac_fixed = samp_frac_fixed,
  time_increment = 0.2,
  f_seg = f_seg,
  R0_prior = c(log(4.7), 0.2),
  iter = 150,
  i0 = 6,
  seed = 123,
  # delay_scale = 7,
  # delay_shape = 1.73,
  pars = c(N = 19.45e6, D = 5, k1 = 1/5, k2 = 1,
    q = 0.05, r = 0.1, ur = 0.02, f0 = 1,
    start_decline = 11, end_decline = 25),
  chains = 5)
ny_fit
p <- covidseir::project_seir(ny_fit, iter = 1:50)
covidseir::tidy_seir(p, resample_y_rep = 30) %>%
  covidseir::plot_projection(new_york)

# k1 -------------------------------------------------

# sampFrac -------------------------------------

vec <- c(0.1, 0.2, 0.25, 0.3, 0.4)
m5 <- furrr::future_map(vec, function(x) {
  f_seg <- c(rep(0, 11), rep(1, nrow(new_york) - 11))
  samp_frac_fixed <- rep(x, nrow(new_york))
  new_york_fit <- covidseir::fit_seir(
    daily_cases = new_york$value,
    samp_frac_fixed = samp_frac_fixed,
    time_increment = 0.2,
    f_seg = f_seg,
    R0_prior = c(log(4.7), 0.2),
    iter = 200,
    i0 = 6 * 1,
    seed = 42,
    # delay_scale = 7,
    # delay_shape = 1.73,
    pars = c(N = 19.45e6, D = x, k1 = 1/5, k2 = 1,
      q = 0.05, r = 0.1, ur = 0.02, f0 = 1,
      start_decline = 11, end_decline = 25),
    chains = 1)
  new_york_fit
})

g <- purrr::map2(m5, vec, function(x, y) {
  p <- covidseir::project_seir(x, iter = 1:50)
  covidseir::tidy_seir(p, resample_y_rep = 30) %>%
    covidseir::plot_projection(new_york) +
    ggtitle(y)
})

cowplot::plot_grid(plotlist = g)

