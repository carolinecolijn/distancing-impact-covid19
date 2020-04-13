#' Lambda_d: returns expected number of cases on day d
#' @author Caroline Colijn, Jessica Stockdale
getlambd <- function(out,
                     pars,
                     day,
                     data = bcdata,
                     sampFrac = 0.1,
                     delayShape = 1.73,
                     delayScale = 9.85) {
  meanDelay <- delayScale * gamma(1 + 1 / delayShape)
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
  ii <- which(out$time > day - 60 & out$time <= day)
  dx <- out$time[ii[2]] - out$time[ii[1]] # assumes equal time intervals
  # all new cases arising at each of those times
  incoming <- with(pars, {
    k2 * (out$E2[ii] + out$E2d[ii])
  })
  march15_modelform <- data$day[which(data$Date == as.Date("2020-03-14"))]
  thisSamp <- ifelse(day < march15_modelform,
    sampFrac,
    sampFrac * pars$ratio
  )
  # print(thisSamp)
  # each of the past times' contribution to this day's case count
  ft <- thisSamp * incoming * dweibull(
    x = max(out$time[ii]) - out$time[ii],
    shape = delayShape,
    scale = delayScale
  )
  # return numerical integral of ft
  return(0.5 * (dx) * (ft[1] + 2 * sum(ft[2:(length(ft) - 1)]) + ft[length(ft)]))
}

#' @title Social Distancing Model
#' @author Caroline Colijn
#' @description SEIR-type model with time-dependent social distancing. Social distancing reduces
#'  frequency of contact. Individuals can move between distanced and not distanced compartments.
#' @param t time
#' @param state (S, E1, E2, I, Q, R, Sd, E1d, E2d, Id, Qd, Rd)
#'   S: Susceptible, E1: Exposed but not infectious, E2: Exposed and Infectious, I: Infectious,
#'   can be quarantined, R: Removed. The d compartments denote socially distanced individuals.
#' @param pars (N, D, R0, k1, k2, q, r, ur, f)
#'   f: strength of social distancing, r/(r+ur): frac of population who are distancing
#' @param sdtiming timing of social distancing
#' @return time derivatives for input to ODE solver

socdistmodel <- function(t,
                         state,
                         parms,
                         sdtiming) {
  with(as.list(c(
    state,
    parms
  )), {
    f <- sdtiming(t, f1 = parms$f1, f2 = parms$f2)
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

#' Linear decrease in f between two time points
#' @author Andrew Edwards
sdtiming_gradual <- function(t,
                             start_decline = 15, # start the decline the next day
                             end_decline = 22, # end decline at f2
                             f1 = pars$f1, # f value before decline
                             f2 = pars$f2) { # f value after decline
  if (t < start_decline) {
    return(f1)
  }
  if (t >= start_decline & t < end_decline) {
    return(f2 + (end_decline - t) * (f1 - f2) / (end_decline - start_decline))
  }
  if (t >= end_decline)  {
    return(f2)
  }
}

reproject_fits <- function(.R0, .f2, .phi, .i, obj, .sdfunc = sdtiming_gradual,
  .time = NULL, return_ode_dat = FALSE, pars = list(
    N = 4.4e6,
    D = 4,
    R0 = 2.6,
    k1 = 1 / 4,
    k2 = 1,
    q = 0.05,
    r = 1,
    ur = 0.2,
    f1 = 1.0,
    f2 = 0.4,
    ratio = 0.3/0.1 # 2nd stage sampFrac
  )) {
  .pars <- pars
  .pars$R0 <- .R0
  .pars$f2 <- .f2
  if (is.null(.time)) {
    .time <- obj$time
  }
  max_day <- max(.time)
  .d <- as.data.frame(deSolve::ode(
    y = obj$state_0,
    times = .time,
    func = socdistmodel,
    parms = .pars,
    method = "rk4",
    sdtiming = .sdfunc))
  dat <- data.frame(
    Date = seq(lubridate::ymd("2020-03-01"),
      lubridate::ymd("2020-03-01") + max_day,
      by = "day"
    )
  )
  dat$day <- seq_along(dat$Date)
  mu <- purrr::map_dbl(seq(1, max_day), function(x) {
    getlambd(.d, pars = .pars, data = dat, day = x)
  })
  out <- data.frame(
    day = seq(1, max_day),
    lambda_d = mu,
    y_rep = MASS::rnegbin(max_day, mu, theta = .phi),
    iterations = .i,
    R0 = .R0, f2 = .f2, phi = .phi
  )
  if (return_ode_dat)
    return(dplyr::mutate(.d, iterations = .i))
  else
    return(out)
}
