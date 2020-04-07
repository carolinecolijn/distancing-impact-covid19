library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

wd <- getwd()
setwd(here::here("selfIsolationModel", "stan"))

x_r <- c(
  N = 4.4e6 / 1e3, # population of BC in thousands
  D = 5,
  k1 = 1 / 5,
  k2 = 1,
  q = 0.05,
  r = 1,
  ur = 0.4,
  f1 = 1.0,
  f2 = 0.4,
  start_decline = 12,
  end_decline = 22
)

# Original data loading:
bcdata <- read.csv(here::here("bc_casecounts1803.csv"), stringsAsFactors = FALSE)
bcdata$Date <- lubridate::dmy(bcdata$Date)
bcdata$daily_diffs <- c(bcdata$Cases[2] - bcdata$Cases[1], diff(bcdata$Cases))
bcdata$day <- seq_len(nrow(bcdata))

# New data loading (from file being updated every day)
new_data = read.csv(here::here("nCoVDailyData/CaseCounts/BC Case counts.csv"),
                    header = TRUE) %>%
  dplyr::as_tibble()
names(new_data)[names(new_data) == "BC"] = "Cases"
new_data$Date = lubridate::dmy(new_data$Date)

new_data$day <- seq_len(nrow(new_data))
new_data$daily_diffs <- c(new_data$Cases[2] - new_data$Cases[1],
                          diff(new_data$Cases))
# numbers match bcdata, plus the extra updated days (early and late)

# See SIR-explore.Rmd regarding including number of tests


# daily_diffs <- bcdata$daily_diffs
# TODO: fudge this for now to give same start date as bcdata (which the initial conditions
# have been tuned to somewhat), need to include 'start_fit_data' etc. settings
# setup in SIR-expolore.Rmd
daily_diffs <- dplyr::filter(new_data, Date >= "2020-03-01")$daily_diffs

fsi <- x_r[["r"]] / (x_r[["r"]] + x_r[["ur"]])
nsi <- 1 - fsi
i0 <- 8

state_0 <- c(
  S = nsi * (x_r[["N"]] - i0),
  E1 = 0.4 * nsi * i0,
  E2 = 0.1 * nsi * i0,
  I = 0.5 * nsi * i0,
  Q = 0,
  R = 0,
  Sd = fsi * (x_r[["N"]] - i0),
  E1d = 0.4 * fsi * i0,
  E2d = 0.1 * fsi * i0,
  Id = 0.5 * fsi * i0,
  Qd = 0,
  Rd = 0
)

stopifnot(
  names(x_r) ==
    c("N", "D", "k1", "k2", "q", "r", "ur", "f1", "f2", "start_decline", "end_decline")
)
stopifnot(
  names(state_0) == c("S", "E1", "E2", "I", "Q", "R", "Sd", "E1d", "E2d", "Id", "Qd", "Rd")
)

forecast_days <- 25
time_increment <- 0.5
days <- seq(1, length(daily_diffs) + forecast_days)
last_day_obs <- length(daily_diffs)
time <- seq(-30, max(days) + forecast_days, time_increment)
last_time_obs <- max(which(time < last_day_obs)) # FIXME: + 1?

get_time_id <- function(day, time) max(which(time < day))
time_day_id <- vapply(days, get_time_id, numeric(1), time = time)

get_time_day_id0 <- function(day, time, days_back) {
  check <- time < (day - days_back)
  if (sum(check) == 0L) {
    1L
  } else {
    max(which(check))
  }
}
time_day_id0 <- vapply(days, get_time_day_id0, numeric(1),
  time = time, days_back = 45L)
# needs to be at least 40 or it starts affecting the results

sampFrac <- ifelse(seq_along(time) < time_day_id[14], 0.35, 0.35 * 2)

# Informative R0 prior example:
# https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-3-transmissibility-of-covid-19/
# 2.6 (uncertainty range: 1.5-3.5)
# x <- rlnorm(1e5, meanlog = log(2.55), sdlog = 0.2)
# mean(x)
# quantile(x, probs = c(0.025, 0.5, 0.975))
R0_prior <- c(log(2.6), 0.2)

stan_data <- list(
  T = length(time),
  days = days,
  daily_diffs = daily_diffs,
  offset = rep(log(1), length(days)),
  N = length(days),
  y0 = state_0,
  t0 = min(time) - 1,
  time = time,
  x_r = x_r,
  delayShape = 1.972,
  delayScale = 12.053,
  sampFrac = sampFrac,
  time_day_id = time_day_id,
  time_day_id0 = time_day_id0,
  R0_prior = R0_prior,
  phi_prior = c(log(1), 0.5),
  priors_only = 0L,
  last_day_obs = last_day_obs
)

seeiqr_model <- stan_model("seeiqr.stan")
map_estimate <- optimizing(
  seeiqr_model,
  data = stan_data
)
map_estimate$par['theta[1]']
map_estimate$par['phi']

initf <- function() {
  list(
    theta = array(rlnorm(1, log(map_estimate$par[['theta[1]']]), 0.2)),
    phi = rlnorm(1, log(map_estimate$par[['phi']]), 0.1)
  )
}
fit <- sampling(
  seeiqr_model,
  data = stan_data,
  iter = 1000L,
  chains = 4L,
  init = initf,
  seed = 4, # https://xkcd.com/221/
  pars = c("theta", "phi", "lambda_d", "y_hat", "y_rep")
)
# saveRDS(fit, file = "sir-fit.rds")

print(fit, pars = c("theta", "phi"))
# Using bcdata:
#> source("02-seeiqr-fit.R")
# Inference for Stan model: seeiqr.
# 4 chains, each with iter=1000; warmup=500; thin=1;
# post-warmup draws per chain=500, total post-warmup draws=2000.

#         mean se_mean   sd 2.5%  25%  50%  75% 97.5% n_eff Rhat
# theta[1] 2.57    0.00 0.09 2.41 2.51 2.57 2.62  2.76  1427    1
# phi      1.47    0.01 0.42 0.83 1.18 1.41 1.69  2.44  1503    1

# Using new_data:
# > source("02-seeiqr-fit.R")
# recompiling to avoid crashing R session
# Inference for Stan model: seeiqr.
# 4 chains, each with iter=1000; warmup=500; thin=1;
# post-warmup draws per chain=500, total post-warmup draws=2000.
#
#          mean se_mean   sd 2.5% 25%  50%  75% 97.5% n_eff Rhat
# theta[1] 2.55    0.00 0.08 2.40 2.5 2.55 2.60  2.73  1030    1
# phi      1.61    0.01 0.44 0.91 1.3 1.55 1.85  2.65  1153    1
#
# new_data has extra data so expect small change in results, looks okay though
# n_eff reduced. BUT new_data has those new low values that I think weren't bcdata.



post <- rstan::extract(fit)

setwd(wd)
