library(rstan)
library(dplyr)
library(ggplot2)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
theme_set(ggsidekick::theme_sleek()) # devtools::install_github("seananderson/ggsidekick")
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
.today <- max(dat$Date)
dat <- dplyr::filter(dat, Date >= "2020-03-01")
daily_diffs <- dat$daily_diffs
if (.today == "2020-04-08") {
  daily_diffs <- c(daily_diffs, 34, 40, 35)
  .today <- "2020-04-11"
}
# plot(daily_diffs, type = "o")
seeiqr_model <- stan_model("seeiqr.stan")
