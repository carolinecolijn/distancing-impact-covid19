library(dplyr)
library(ggplot2)
dmy <- lubridate::dmy
ymd <- lubridate::ymd
options(mc.cores = parallel::detectCores() / 2)

# https://en.wikipedia.org/wiki/COVID-19_pandemic_in_New_Zealand#Requirements
# Ardern announced that, effective 01:00 on 16 March, all travellers arriving in or returning to New Zealand from outside of the country must self-isolate for 14 days.
# In addition, restrictions were placed on travel to the Pacific islands from New Zealand, barring travel to the region by those showing signs of coronavirus symptoms, as well as close contacts of coronavirus patients.

# On 16 March, Ardern called for a halt to public gatherings of more than 500 people and warned that the outbreak could lead to a recession greater than the 2008 global financial crisis.[223][224]

# On 19 March, the Government required the cancellation of indoor gatherings of more than 100 people.

# On 23 March, Ardern raised the alert level to three and announced the closure of all schools, beginning that day. She also announced that the alert level would rise to four at 11:59pm on 25 March, instituting a nationwide lockdown.

# g <- readr::read_csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=722f3143b586a83f")
# g <- filter(g, country_region == "New Zealand")
# ggplot(g, aes(date, transit_stations_percent_change_from_baseline)) +
#   geom_point() +
#   # geom_vline(xintercept = ymd("2020-03-16")) +
#   geom_vline(xintercept = ymd("2020-03-18")) +
#   # geom_vline(xintercept = ymd("2020-03-24")) +
#   geom_vline(xintercept = ymd("2020-03-27"))

d <- readr::read_csv(here::here("data-raw/covid-cases-7may20-NZ.csv"), skip = 3)
d <- rename(d, date = `Date notified of potential case`, overseas = `Overseas travel`) %>%
  select(date, overseas) %>%
  mutate(date = dmy(date)) %>%
  group_by(date) %>%
  summarize(all_cases = n(),
    not_overseas_cases = sum(overseas == "No", na.rm = TRUE))
d

tidyr::pivot_longer(d, -date) %>%
  ggplot(aes(date, value, colour = name)) +
  geom_line()

# get_days_since <- function(until, since) {
#   abs(as.numeric(difftime(until, since, units = "days")))
# }
# (start_decline <- get_days_since(ymd("2020-03-17"), min(florida$date)))
# (end_decline <- get_days_since(ymd("2020-04-01"), min(florida$date)))
# (f_seg <- c(rep(0, start_decline), rep(1, nrow(florida) - start_decline)))

nz <- filter(d, date >= ymd("2020-03-15"))
nz$all_cases
diff(nz$date)

nz <- left_join(tibble(date = seq(min(nz$date), max(nz$date), by = "1 day")), nz)
nz$all_cases
nz$not_overseas_cases
# nz$not_overseas_cases[is.na(nz$not_overseas_cases)] <- 0
# nz$not_overseas_cases
diff(nz$date)
nz <- nz %>%
  mutate(all_cases = ifelse(!is.na(all_cases), all_cases, 0)) %>%
  mutate(not_overseas_cases = ifelse(!is.na(not_overseas_cases), not_overseas_cases, 0))

nz$all_cases
nz$not_overseas_cases
diff(nz$date)

april1 <- as.numeric(ymd("2020-04-01") - min(nz$date))

# samp_frac_fixed <- c(rep(0.4, april1 - 1), rep(0.6, 9999))
samp_frac_fixed <- c(rep(0.4, nrow(nz)))

# samp_frac_fixed <- c(0.3, 0.3, 0.3, 0.3, 0.3, seq(0.3, 0.6, length.out = 12), rep(0.6, 100))
samp_frac_fixed <- samp_frac_fixed[1:nrow(nz)]
plot(samp_frac_fixed)

.s <- as.numeric(ymd("2020-03-18") - min(nz$date))
.e <- as.numeric(ymd("2020-03-26") - min(nz$date))

# (f_seg <- c(rep(0, .s), rep(1, nrow(nz) - .s)))

# https://www.stats.govt.nz/topics/population # 4951500

fit <- covidseir::fit_seir(
  daily_cases = nz$not_overseas_cases,
  samp_frac_fixed = samp_frac_fixed,
  time_increment = 0.1,
  R0_prior = c(log(2.6), 0.2),
  f_prior = c(0.3, 0.2),
  iter = 400,
  # f_ramp_rate = 0.5,
  start_decline_prior = c(log(.s), 0.1),
  end_decline_prior = c(log(.e), 0.1),
  chains = 6,
  i0 = 0.001,
  delay_shape = 1.53,
  delay_scale = 7.828,
  pars = c(N = 4951500, D = 5, k1 = 1/5, k2 = 1,
    q = 0.05, r = 0.1, ur = covidseir:::getu(0.95, r = 0.1), f0 = 1))
print(fit)

nz$day <- seq_len(nrow(nz))
nz$value <- nz$not_overseas_cases
# nz$value <- nz$all_cases
p <- covidseir::project_seir(fit, iter = 1:100, forecast_days = 0)

covidseir::tidy_seir(p) %>%
  covidseir::plot_projection(nz)

# source(here::here("analysis/plot_projection_w_inset.R"))
# plot_projection_w_inset(p, nz, obj = fit)

saveRDS(fit, file = here::here("data-generated/nz-fit.rds"))
saveRDS(nz, file = here::here("data-generated/nz-dat.rds"))

# (.s <- as.numeric(ymd("2020-03-18") - min(nz$date)))
# (.e <- as.numeric(ymd("2020-03-27") - min(nz$date)))
# (.e <- as.numeric(ymd("2020-03-18") - min(nz$date)))
#
# model <- rstan::stan_model("analysis/seeiqr.stan")
# source("analysis/fit_seeiqr.R")
# source("analysis/make_projection_plot.R")


#
# m <- list()
# m[[1]] <- fit_seeiqr(
#   nz$not_overseas_cases,
#   seeiqr_model = model,
#   forecast_days = 0,
#   R0_prior = c(log(2.5), 0.2),
#   chains = 3,
#   iter = 130,
#   i0 = 0.05,
#   delayShape = 1.48,
#   delayScale = 7.93,
#   sampled_fraction1 = 0.4,
#   sampled_fraction2 = 0.4,
#   pars = c(N = 4951500, D = 5, k1 = 1/5, k2 = 1,
#     q = 0.05, r = 0.1, ur = 0.02, f1 = 1,
#     start_decline = 3, end_decline = 10))
# m[[2]] <- fit_seeiqr(
#   nz$not_overseas_cases,
#   seeiqr_model = model,
#   forecast_days = 0,
#   R0_prior = c(log(2.5), 0.2),
#   chains = 3,
#   iter = 130,
#   i0 = 0.05,
#   delayShape = 1.48,
#   delayScale = 7.93,
#   sampled_fraction1 = 0.4,
#   sampled_fraction2 = 0.4,
#   pars = c(N = 4951500, D = 5, k1 = 1/5, k2 = 1,
#     q = 0.05, r = 0.1, ur = 0.02, f1 = 1,
#     start_decline = 3, end_decline = 12))
# m[[3]] <- fit_seeiqr(
#   nz$not_overseas_cases,
#   seeiqr_model = model,
#   forecast_days = 0,
#   R0_prior = c(log(2.5), 0.2),
#   chains = 3,
#   iter = 130,
#   i0 = 0.05,
#   delayShape = 1.48,
#   delayScale = 7.93,
#   sampled_fraction1 = 0.4,
#   sampled_fraction2 = 0.4,
#   pars = c(N = 4951500, D = 5, k1 = 1/5, k2 = 1,
#     q = 0.05, r = 0.1, ur = 0.02, f1 = 1,
#     start_decline = 3, end_decline = 14))
#
# gg <- lapply(m, function(x) make_projection_plot(list(x), first_date = as.character(min(nz$date))))
# cowplot::plot_grid(plotlist = gg)
#
# fit2 <- fit_seeiqr(nz$not_overseas_cases, seeiqr_model = model,
#   forecast_days = 0,
#   R0_prior = c(log(2.5), 0.2),
#   chains = 6,
#   iter = 130,
#   i0 = 1,
#   delayShape = 1.48,
#   delayScale = 7.93,
#   sampled_fraction1 = 0.4,
#   sampled_fraction2 = 0.4,
#   pars = c(N = 4951500, D = 5, k1 = 1/5, k2 = 1,
#     q = 0.05, r = 0.1, ur = 0.02, f1 = 1,
#     start_decline = .s, end_decline = .e))
# print(fit2$fit, pars = c("R0", "f2", "phi"))

# make_projection_plot(list(fit2), first_date = as.character(min(nz$date)))
