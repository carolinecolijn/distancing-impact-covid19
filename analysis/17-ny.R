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

new_york <- filter(d, state %in% "NY") %>%
  select(date, positiveIncrease, totalTestResultsIncrease, hospitalizedIncrease) %>%
  filter(date >= ymd("2020-03-05")) %>%
  rename(value = positiveIncrease, tests = totalTestResultsIncrease, hospitalized = hospitalizedIncrease) %>%
  arrange(date) %>%
  mutate(day = seq_len(n()))

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

# https://en.wikipedia.org/wiki/COVID-19_pandemic_in_New_York_(state)
# On March 20, de Blasio called for drastic measures to combat the coronavirus outbreak. "We have to go to a shelter-in-place model," he said, praising California's "stay at home" model for sheltering in place.[91]

# Cuomo also on March 28 ordered all nonessential construction sites in the state to shut down. This led the developers of the Legoland park under construction in Goshen to postpone their planned July 4 opening date until 2021. A specific date was not set, but Orange County's director of tourism expected it would probably be the normal April opening date.[112]

new_york
# View(new_york)

plot(new_york$day, new_york$value, type = "o")
plot(new_york$day, new_york$tests, type = "o")
plot(new_york$date, new_york$value, type = "l")
lines(new_york$date, new_york$hospitalized, col = "red")
lines(new_york$date, new_york$tests/10, col = "blue")

.s <- as.numeric(ymd("2020-03-13") - min(new_york$date))
.e <- as.numeric(ymd("2020-03-28") - min(new_york$date))

# g <- readr::read_csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=722f3143b586a83f")
# g1 <- filter(g, country_region == "United States")
# g1 <- filter(g, sub_region_1 == "New York")
# ggplot(g1, aes(date, transit_stations_percent_change_from_baseline)) +
#   geom_point() +
#   # geom_vline(xintercept = ymd("2020-03-16")) +
#   geom_vline(xintercept = ymd("2020-03-13")) +
#   # geom_vline(xintercept = ymd("2020-03-24")) +
#   geom_vline(xintercept = ymd("2020-03-28"))

# Tests Jump on day 9 from <100 to >2000
# and to > 10,000 by the 16th

(samp_frac_fixed <- rep(0.25, nrow(new_york)))
# (f_seg <- c(rep(0, 11), rep(1, nrow(new_york) - 11)))

ny_fit <- covidseir::fit_seir(
  daily_cases = new_york$value,
  samp_frac_fixed = samp_frac_fixed,
  time_increment = 0.1,
  R0_prior = c(log(2.6), 0.2),
  iter = 500,
  chains = 8,
  start_decline_prior = c(log(.s), 0.2),
  end_decline_prior = c(log(.e), 0.2),
  i0 = 0.5,
  pars = c(N = 19.45e6, D = 5, k1 = 1/5, k2 = 1,
    q = 0.05, r = 0.1, ur = 0.02, f0 = 1
    ))
ny_fit
p <- covidseir::project_seir(ny_fit, iter = 1:100)
covidseir::tidy_seir(p) %>%
  covidseir::plot_projection(new_york)

saveRDS(ny_fit, file = here::here("data-generated/new-york-fit.rds"))
saveRDS(new_york, file = here::here("data-generated/new-york-dat.rds"))

# model <- rstan::stan_model("analysis/seeiqr.stan")
# source("analysis/fit_seeiqr.R")
# source("analysis/make_projection_plot.R")
#
# ny_fit <- fit_seeiqr(new_york$value,
#   seeiqr_model = model,
#   forecast_days = 30,
#   R0_prior = c(log(4), 0.2),
#   chains = 3,
#   iter = 160,
#   i0 = 2,
#   sampled_fraction1 = 0.25,
#   sampled_fraction2 = 0.25,
#   pars = c(N = 19.45e6, D = 5, k1 = 1/5, k2 = 1,
#     q = 0.05, r = 0.1, ur = 0.02, f1 = 1,
#     start_decline = 12, end_decline = 25))
# print(ny_fit$fit, pars = c("R0", "f2", "phi"))
#
# make_projection_plot(list(ny_fit), first_date = as.character(min(new_york$date)))
