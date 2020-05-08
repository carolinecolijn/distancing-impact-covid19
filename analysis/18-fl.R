library(ggplot2)
library(dplyr)
# library(future)
library(covidseir)
# plan(multisession)
options(mc.cores = parallel::detectCores() / 2)
ymd <- lubridate::ymd

d <- readr::read_csv("https://covidtracking.com/api/v1/states/daily.csv")
readr::write_csv(d, "data-generated/us-data.csv")
d <- readr::read_csv("data-generated/us-data.csv")
d$date <- lubridate::ymd(d$date)

florida <- filter(d, state %in% "FL") %>%
  select(date, positiveIncrease, totalTestResultsIncrease, hospitalizedIncrease) %>%
  filter(date >= ymd("2020-03-05")) %>%
  rename(value = positiveIncrease, tests = totalTestResultsIncrease, hospitalized = hospitalizedIncrease) %>%
  arrange(date) %>%
  mutate(day = seq_len(n()))

florida
# View(florida)

plot(florida$day, florida$value, type = "o")
plot(florida$day, florida$tests, type = "o")
plot(florida$date, florida$value, type = "l")
lines(florida$date, florida$hospitalized, col = "red")
lines(florida$date, florida$tests/10, col = "blue")

get_days_since <- function(until, since) {
  abs(as.numeric(difftime(until, since, units = "days")))
}
(start_decline <- get_days_since(ymd("2020-03-17"), min(florida$date)))
(end_decline <- get_days_since(ymd("2020-04-01"), min(florida$date)))
(f_seg <- c(rep(0, start_decline), rep(1, nrow(florida) - start_decline)))

(samp_frac_fixed <- rep(0.25, nrow(florida)))

fit <- covidseir::fit_seir(
  daily_cases = florida$value,
  samp_frac_fixed = samp_frac_fixed,
  time_increment = 0.1,
  f_seg = f_seg,
  R0_prior = c(log(3), 0.2),
  iter = 200,
  chains = 4,
  i0 = 2,
  pars = c(N = 21.48e6, D = 5, k1 = 1/5, k2 = 1,
    q = 0.05, r = 0.1, ur = 0.02, f0 = 1,
    start_decline = 12, end_decline = 27))
fit
p <- covidseir::project_seir(fit, iter = 1:100)
covidseir::tidy_seir(p) %>%
  covidseir::plot_projection(florida)
