library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
wd <- getwd()
setwd(here::here("selfIsolationModel", "stan"))
source("fit_seeiqr.R")

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
# TODO: fudge this for now to give same start date as bcdata
# (which the initial conditions have been tuned to somewhat).
dat <- dplyr::filter(dat, Date >= "2020-03-01")
daily_diffs <- dat$daily_diffs
plot(daily_diffs)

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
# total_tests <- c(total_tests, rep(total_tests[length(total_tests)], 25))

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

seeiqr_model <- stan_model("seeiqr.stan")

fits <- list()
fits[[1]] <- fit_seeiqr(
  daily_diffs,
  seeiqr_model = seeiqr_model)
fits[[2]] <- fit_seeiqr(
  daily_diffs,
  fixed_f_forecast = 1,
  seeiqr_model = seeiqr_model)
fits[[3]] <- fit_seeiqr(
  daily_diffs,
  sampled_fraction1 = 0.3,
  sampled_fraction2 = 0.3,
  seeiqr_model = seeiqr_model,
  forecast_days = 90)
fits[[4]] <- fit_seeiqr(
  daily_diffs,
  sampled_fraction1 = 0.3,
  sampled_fraction2 = 0.3,
  fixed_f_forecast = 0.2,
  seeiqr_model = seeiqr_model,
  forecast_days = 90)

# e.g.
# print(fits[[1]], pars = c("R0", "f2", "phi"))

setwd(wd)
