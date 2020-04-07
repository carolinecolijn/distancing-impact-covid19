library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
wd <- getwd()
setwd(here::here("selfIsolationModel", "stan"))
source("fit_seeiqr.R")

new_data <- read.csv(here::here("nCoVDailyData/CaseCounts/BC Case counts.csv")) %>%
  dplyr::as_tibble()
names(new_data)[names(new_data) == "BC"] <- "Cases"
new_data$Date <- lubridate::dmy(new_data$Date)

new_data$day <- seq_len(nrow(new_data))
new_data$daily_diffs <- c(
  new_data$Cases[2] - new_data$Cases[1],
  diff(new_data$Cases)
)
# TODO: fudge this for now to give same start date as bcdata
# (which the initial conditions have been tuned to somewhat).
daily_diffs <- dplyr::filter(new_data, Date >= "2020-03-01")$daily_diffs

## Andy's code, to be integrated...
# Load in number of tests each day:
# Crude for now - want to check how the numbers of cases (positive tests)
# compare with the ones in new_data. Could scale them up perhaps.
# load(paste0(here::here(),
#   "/nCoVDailyData/Labdata/testsanonym.RData"))
# # Only contains dataframe 'testsanonymized'
#
# tests_anon <- dplyr::as_tibble(testsanonymized) %>%
#   type.convert() %>%
#   dplyr::mutate(results_date = date(results_date))
#
# tests_by_day <- tests_anon %>%
#   dplyr::group_by(results_date) %>%
#   dplyr::count(name = "total_tests")
# # gives dateframe of date and total_tests
#
# TODO: setup-dates.R explains how Andy is setting up the dates (it's mostly
# explanations that I didn't want to clutter up here).
# Load in the detailed case data of estimated times between people's onset of
# symptoms and their test becoming a 'reported case'. Ceate delay_data tibble(),
# where for the negative binomial model you want the time_to_report column (may
# need as.numeric() as they are in days).
# source(here::here("/selfIsolationModel/SIR-functionalised/funcs.R"))
# # Just need the one function:
# delay_data <- load_tidy_delay_data()[["delay_data"]]

m <- fit_seeiqr(daily_diffs)
