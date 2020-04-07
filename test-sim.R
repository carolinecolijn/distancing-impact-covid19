pars_default <- list(
  N = 4.4e6,
  D = 5, #
  R0 = 2.65, #
  k1 = 1 / 5, #
  k2 = 1, #
  q = 0.05, #
  r = 1, #
  ur = 0.4, #
  f1 = 1.0, # initial value of f, educated guess
  f2 = 0.4, # final value of f, educated guess
  ratio = 2
)
fsi <- with(
  pars_default,
  r / (r + ur)
)
nsi <- 1 - fsi
i0 <- 8
state_0 <- c(
  S = nsi * (pars_default$N - i0),
  E1 = 0.4 * nsi * i0,
  E2 = 0.1 * nsi * i0,
  I = 0.5 * nsi * i0,
  Q = 0,
  R = 0,
  Sd = fsi * (pars_default$N - i0),
  E1d = 0.4 * fsi * i0,
  E2d = 0.1 * fsi * i0,
  Id = 0.5 * fsi * i0,
  Qd = 0,
  Rd = 0
)

sdtiming_gradual <- function(t,
                             start_decline = 13, # start the decline the next day
                             end_decline = 22, # end decline at f2
                             f1 = 1, # f value before decline
                             f2 = 0.4) { # f value after decline
  if (t < start_decline) {
    return(f1)
  }
  if (t >= start_decline & t < end_decline) {
    return(f2 + (end_decline - t) * (f1 - f2) / (end_decline - start_decline))
  }
  if (t >= end_decline) {
    return(f2)
  }
}

socdistmodel <- function(t,
                         state,
                         parms,
                         sdtiming) {
  with(as.list(c(
    state,
    parms
  )), {
    f <- sdtiming(t, f1 = f1, f2 = f2)
    dSdt <- -(R0 / (D + 1 / k2)) * (I + E2 + f * (Id + E2d)) * S / N - r * S + ur * Sd
    dE1dt <- (R0 / (D + 1 / k2)) * (I + E2 + f * (Id + E2d)) * S / N - k1 * E1 - r * E1 + ur * E1d
    dE2dt <- k1 * E1 - k2 * E2 - r * E2 + ur * E2d
    dIdt <- k2 * E2 - q * I - I / D - r * I + ur * Id
    dQdt <- q * I - Q / D - r * Q + ur * Qd
    dRdt <- I / D + Q / D - r * R + ur * Rd

    dSddt <- -(f * R0 / (D + 1 / k2)) * (I + E2 + f * (Id + E2d)) * Sd / N + r * S - ur * Sd
    dE1ddt <- (f * R0 / (D + 1 / k2)) * (I + E2 + f * (Id + E2d)) * Sd / N - k1 * E1d + r * E1 - ur * E1d
    dE2ddt <- k1 * E1d - k2 * E2d + r * E2 - ur * E2d
    dIddt <- k2 * E2d - q * Id - Id / D + r * I - ur * Id
    dQddt <- q * Id - Qd / D + r * Q - ur * Qd
    dRddt <- Id / D + Qd / D + r * R - ur * Rd
    # dRdt = I/D; dr + ds + di =0, S+I+R = N --> R = N-S-I and we eliminate R
    list(c(
      dSdt,
      dE1dt,
      dE2dt,
      dIdt,
      dQdt,
      dRdt,
      dSddt,
      dE1ddt,
      dE2ddt,
      dIddt,
      dQddt,
      dRddt
    ))
  })
}

getlambd <- function(out,
                     pars,
                     day,
                     data = bcdata,
                     sampFrac = 0.35,
                     delayShape = 1.9720199,
                     delayScale = 12.0529283) {
  meanDelay <- delayScale * gamma(1 + 1 / delayShape)
  # = 10.685 with parameters 1.972... and 12.0529...
  try(if (var(diff(out$time)) > 0.005) {
    stop("approx integral assumes equal time steps")
  })
  try(if (max(out$time) < day) {
    stop("model simulation is not long enough for the data")
  })
  try(if (min(out$time) > day - (2 * meanDelay + 1)) {
    stop("we need an earlier start time for the model")
  })

  # relevant times to identify new cases
  ii <- which(out$time > day - 2 * meanDelay & out$time <= day) # then just
  # need dweibull over this range
  dx <- out$time[ii[2]] - out$time[ii[1]] # assumes equal time intervals

  # all new cases arising at each of those times
  incoming <- with(pars, {
    k2 * (out$E2[ii] + out$E2d[ii])
  })

  march15_modelform <- data$day[which(data$Date == as.Date("2020-03-14"))]
  # march15_modelform <- 14
  thisSamp <- ifelse(day < march15_modelform,
    sampFrac,
    sampFrac * pars$ratio
  ) # here is ratio. it's in pars.

  # each of the past times' contribution to this day's case count
  ft <- thisSamp * incoming * dweibull(
    x = max(out$time[ii]) - out$time[ii], # values over which to integrate (correct?)
    shape = delayShape,
    scale = delayScale
  )

  # return numerical integral of ft
  return(0.5 * (dx) * (ft[1] + 2 * sum(ft[2:(length(ft) - 1)]) + ft[length(ft)]))
  # aha - just trapeziod rule for integrating?
}

example_simulation <- as.data.frame(
  deSolve::ode(
    y = state_0,
    times = seq(0, 91, 0.1),
    func = socdistmodel,
    parms = pars_default,
    sdtiming = sdtiming_gradual
  )
)

dat <- data.frame(
  Date = seq(lubridate::ymd("2020-03-01"),
    lubridate::ymd("2020-04-01"), by = "day"))
dat$day <- seq_along(dat$Date)
lambda_d <- sapply(seq(30, 91), function(x) {
  getlambd(example_simulation, pars = pars_default, data = dat, day = x)
})

plot(seq(-29, 32), lambda_d)

sim_dat <- data.frame(day = 1:32,
  lambda_d = lambda_d[31:62], obs = rpois(32, lambda_d[31:62]))
