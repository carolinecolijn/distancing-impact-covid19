library(Rcpp)
library(rstan)
setwd(here::here("selfIsolationModel/stan"))
my_path <- paste0(here::here(), "/selfIsolationModel/")
source("functions_sir.R")
pars <- list(
  N = 4.4e6, # population of BC
  D = 4,
  R0 = 2.65,
  k1 = 1 / 4,
  k2 = 1,
  q = 0.05,
  r = 1,
  ur = 0.2,
  f1 = 1.0,
  f2 = 0.4,
  ratio = 0.7/0.2 # 2nd stage sampFrac
)
fsi <- with(
  pars,
  r / (r + ur)
)
nsi <- 1 - fsi
i0 <- 8
state_0 <- c(
  S = nsi * (pars$N - i0),
  E1 = 0.4 * nsi * i0,
  E2 = 0.1 * nsi * i0,
  I = 0.5 * nsi * i0,
  Q = 0,
  R = 0,
  Sd = fsi * (pars$N - i0),
  E1d = 0.4 * fsi * i0,
  E2d = 0.1 * fsi * i0,
  Id = 0.5 * fsi * i0,
  Qd = 0,
  Rd = 0
)
times <- seq(
  from = -30,
  to = 20,
  by = 0.5
)

example_simulation <- as.data.frame(deSolve::ode(
  y = state_0,
  times = times,
  func = socdistmodel,
  parms = pars,
  sdtiming = sdtiming_gradual))

source("fit_seeiqr.R")
seeiqr_model <- stan_model("seeiqr.stan")
m <- fit_seeiqr(
  daily_cases = c(0, 0, 1, 3, 1, 8, 0, 6, 5, 0, 7, 7, 18, 9, 22, 38, 53, 45,
    40, 77, 76, 48, 67, 78, 42, 66, 67, 92, 16, 70, 43, 53, 55, 53,
    29, 26, 37, 25, 45, 34, 40, 35),
  seeiqr_model = seeiqr_model,
  forecast_days = 1,
  R0_prior = c(log(2.65), 0.2),
  f2_prior = c(0.4, 0.15),
  iter = 1, algorithm = "Fixed_param",
  chains = 1, cores = 1,
  time_increment = 0.1, days_back = 999
)

getlambd_stan <- function(time, E2, E2d, time_day_id0, time_day_id, sampFrac,
  k2 = 1, delayScale = 9,
  delayShape = 2) {
  .T <- length(time)
  N <- max(time)
  ft <- vector("numeric", length = .T)
  lambda_d <- vector("numeric", length = N)
  dx <- time[2] - time[1]
  for (.t in 1:.T) {
    ft[.t] <- 0
  }
  for (.n in 1:N) {
    thisSamp <- sampFrac[.n]
    for (.t in time_day_id0[.n]:time_day_id[.n]) {
      ft[.t] <- thisSamp * k2 * (E2 + E2d) *
        dweibull(time[time_day_id[.n]] - time[.t], delayShape, delayScale)
    }
    sum_ft_inner <- 0
    for (.t in (time_day_id0[.n] + 1):(time_day_id[.n] - 1)) {
      sum_ft_inner <- sum_ft_inner + ft[.t]
    }
    lambda_d[.n] <- 0.5 * dx * (ft[time_day_id0[.n]] + 2 * sum_ft_inner + ft[time_day_id[.n]])
  }
  lambda_d
}

getlambd_R <- function(time,
  E2, E2d, k2 = 1,
  ratio,
  day,
  sampFrac = 0.2,
  delayShape = 2,
  delayScale = 9) {
  meanDelay <- delayScale * gamma(1 + 1 / delayShape)
  ii <- which(time > day - 999 & time <= day)
  dx <- time[2] - time[1]
  incoming <- k2 * (E2 + E2d)
  thisSamp <- ifelse(day < 12,
    sampFrac,
    sampFrac * ratio
  )
  ft <- thisSamp * incoming * dweibull(
    x = max(time[ii]) - time[ii],
    shape = delayShape,
    scale = delayScale
  )
  0.5 * (dx) * (ft[1] + 2 * sum(ft[2:(length(ft) - 1)]) + ft[length(ft)])
}

.stan <- getlambd_stan(
  time = m$time, E2 = 300, E2d = 20,
  time_day_id0 = m$stan_data$time_day_id0,
  time_day_id = m$stan_data$time_day_id,
  sampFrac = c(rep(0.2, 11), rep(0.7, 99)),
  # sampFrac = m$stan_data$sampFrac,
  k2 = 1, delayScale = 9, delayShape = 2
)

.R <- sapply(1:max(m$time), function(i) {
  getlambd_R(day = i, time = m$time, E2 = 300,
    E2d = 20, k2 = 1, sampFrac = 0.2, ratio = 0.7 / 0.2)
})

Rcpp::sourceCpp('get_lambd.cpp')
# Rcpp::sourceCpp('get_lambd_R.cpp')
.Cpp <- get_lambd_cpp(
  time = m$time,
  k2 = 1,
  E2 = rep(300, length(m$time)),
  E2d = rep(20, length(m$time)),
  delayShape = 2,
  delayScale = 9,
  N = max(m$time),
  T = length(m$time),
  sampFrac = c(rep(0.2, 11), rep(0.7, 99)),
  time_day_id0 = m$stan_data$time_day_id0 - 1,
  time_day_id = m$stan_data$time_day_id - 1)

stopifnot(identical(round(.R, 9), round(.stan, 9)))
stopifnot(identical(round(.Cpp, 9), round(.stan, 9)))
stopifnot(identical(round(.Cpp, 9), round(.R, 9)))

b <- bench::mark(
  .Cpp = get_lambd_cpp(
    time = m$time,
    k2 = 1,
    E2 = 300,
    E2d = 20,
    delayShape = 2,
    delayScale = 9,
    N = max(m$time),
    T = length(m$time),
    sampFrac = c(rep(0.2, 11), rep(0.7, 99)),
    time_day_id0 = m$stan_data$time_day_id0 - 1,
    time_day_id = m$stan_data$time_day_id - 1),
  R = sapply(1:max(m$time), function(i) {
    getlambd_R(day = i, time = m$time, E2 = 300,
      E2d = 20, k2 = 1, sampFrac = 0.2, ratio = 0.7 / 0.2)
  }),
  check = FALSE
)
b

mu <- get_lambd_cpp(
  time = m$time,
  k2 = 1,
  E2 = 300,
  E2d = 20,
  delayShape = 2,
  delayScale = 9,
  N = max(m$time),
  T = length(m$time),
  sampFrac = c(rep(0.2, 11), rep(0.7, 99)),
  time_day_id0 = m$stan_data$time_day_id0 - 1,
  time_day_id = m$stan_data$time_day_id - 1)

reps <- 10
d <- data.frame(mu = rep(mu, reps), samp = rep(seq_len(reps),
  each = length(mu)), day = rep(seq_along(mu), reps))
d$y_rep <- rnbinom(nrow(d), mu = d$mu, size = 3)
ggplot(d, aes(day, y_rep, group = samp)) + geom_line()
