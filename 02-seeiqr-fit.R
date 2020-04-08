library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
wd <- getwd()
setwd(here::here("selfIsolationModel", "stan"))
source("fit_seeiqr.R")

new_data <- readr::read_csv(here::here("nCoVDailyData/CaseCounts/BC Case counts.csv"))
names(new_data)[names(new_data) == "BC"] <- "Cases"

new_data$Date[71] <- "1/4/2020"
new_data$Date[72] <- "2/4/2020"
new_data$Date <- lubridate::dmy(new_data$Date)

new_data$day <- seq_len(nrow(new_data))
new_data$daily_diffs <- c(
  new_data$Cases[2] - new_data$Cases[1],
  diff(new_data$Cases)
)
# TODO: fudge this for now to give same start date as bcdata
# (which the initial conditions have been tuned to somewhat).

new_data <- dplyr::filter(new_data, Date >= "2020-03-01")
daily_diffs <- new_data$daily_diffs
plot(daily_diffs)

# Load in number of tests each day:
# Crude for now - want to check how the numbers of cases (positive tests)
# compare with the ones in new_data. Could scale them up perhaps.
load(paste0(here::here(),
  "/nCoVDailyData/Labdata/testsanonym.RData"))
# # Only contains dataframe 'testsanonymized'
tests_anon <- dplyr::as_tibble(testsanonymized) %>%
  type.convert() %>%
  dplyr::mutate(results_date = lubridate::date(results_date))

tests_by_day <- tests_anon %>%
  dplyr::group_by(results_date) %>%
  dplyr::count(name = "total_tests")

total_tests <- dplyr::filter(tests_by_day, results_date %in% unique(new_data$Date))$total_tests

length(daily_diffs)
length(total_tests)
diff(new_data$Date)
# Create some fake test numbers that match the end of the data set:

plot(total_tests)
plot(daily_diffs/total_tests)

total_tests <- c(total_tests, rep(total_tests[length(total_tests)], 25))

## Andy's code, to be integrated...
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
m2 <- fit_seeiqr(daily_diffs, daily_tests = total_tests, forecast_days = 25)

print(m$fit, pars = c("R0", "f2", "phi"))
print(m2$fit, pars = c("R0", "f2", "phi"))

setwd(wd)
