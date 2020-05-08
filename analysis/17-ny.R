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

(samp_frac_fixed <- rep(0.25, nrow(new_york)))
(f_seg <- c(rep(0, 11), rep(1, nrow(new_york) - 11)))

ny_fit <- covidseir::fit_seir(
  daily_cases = new_york$value,
  samp_frac_fixed = samp_frac_fixed,
  time_increment = 0.1,
  f_seg = f_seg,
  R0_prior = c(log(4.5), 0.2),
  iter = 500,
  chains = 8,
  i0 = 6,
  pars = c(N = 19.45e6, D = 5, k1 = 1/5, k2 = 1,
    q = 0.05, r = 0.1, ur = 0.02, f0 = 1,
    start_decline = 11, end_decline = 25))
ny_fit
p <- covidseir::project_seir(ny_fit, iter = 1:50)
covidseir::tidy_seir(p) %>%
  covidseir::plot_projection(new_york)
