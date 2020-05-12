library(ggplot2)
library(dplyr)
# library(future)
library(covidseir)
# plan(multisession)
options(mc.cores = parallel::detectCores() / 2)
ymd <- lubridate::ymd

# d <- readr::read_csv("https://covidtracking.com/api/v1/states/daily.csv")
# readr::write_csv(d, here::here("data-generated/us-data.csv"))
d <- readr::read_csv(here::here("data-generated/us-data.csv"))
d$date <- lubridate::ymd(d$date)

wa <- filter(d, state %in% "WA") %>%
  select(date, positiveIncrease, totalTestResultsIncrease, hospitalizedIncrease) %>%
  filter(date >= ymd("2020-03-01")) %>%
  rename(value = positiveIncrease, tests = totalTestResultsIncrease, hospitalized = hospitalizedIncrease) %>%
  arrange(date) %>%
  mutate(day = seq_len(n()))

wa
# View(wa)

plot(wa$day, wa$value, type = "o")
plot(wa$day, wa$tests, type = "o")
plot(wa$date, wa$value, type = "l")
# lines(wa$date, wa$hospitalized, col = "red")
lines(wa$date, wa$tests/10, col = "blue")

(.s <- as.numeric(ymd("2020-03-11") - min(wa$date)))
(.e <- as.numeric(ymd("2020-03-23") - min(wa$date)))

g <- readr::read_csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=722f3143b586a83f")
g1 <- filter(g, country_region == "United States")
g1 <- filter(g, sub_region_1 == "Washington")
ggplot(g1, aes(date, transit_stations_percent_change_from_baseline)) +
  geom_point() +
  geom_vline(xintercept = ymd("2020-03-11")) +
  geom_vline(xintercept = ymd("2020-03-23"))

# Tests Jump on day 9 from <100 to >2000
# and to > 10,000 by the 16th

(samp_frac_fixed <- rep(0.25, nrow(wa)))
# (f_seg <- c(rep(0, 11), rep(1, nrow(new_york) - 11)))

wa$value
stopifnot(unique(wa$value[57:58]) == 0)
wa$value[57:58] <- NA
wa$value
fit <- covidseir::fit_seir(
  daily_cases = wa$value,
  samp_frac_fixed = samp_frac_fixed,
  time_increment = 0.1,
  R0_prior = c(log(2.6), 0.2),
  iter = 500,
  chains = 8,
  start_decline_prior = c(log(.s), 0.1),
  end_decline_prior = c(log(.e), 0.1),
  i0 = 1,
  pars = c(N = 7.6e6, D = 5, k1 = 1/5, k2 = 1,
    q = 0.05, r = 0.1, ur = 0.02, f0 = 1
    ))
fit
p <- covidseir::project_seir(fit, iter = 1:100)
covidseir::tidy_seir(p) %>%
  covidseir::plot_projection(wa) +
  scale_y_log10()

saveRDS(fit, file = here::here("data-generated/washington-fit.rds"))
saveRDS(wa, file = here::here("data-generated/washington-dat.rds"))
