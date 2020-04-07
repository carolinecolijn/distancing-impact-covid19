my_path <- paste0(here::here(), "/selfIsolationModel/")
source(paste0(my_path, "functions_sir.R"))  # libraries to load plus function definitions

pars = list(N = 4.4e6, #population of BC. Another potential issue as BC isn't homogeneously mixing; could fit to Vancouver instead
  D = 5,
  R0 = 2.65,
  k1 = 1/5,
  k2 = 1,
  q = 0.05,
  r = 1,
  ur = 0.4,
  f1 = 1.0,   # initial value of f
  f2 = 0.4,     # final value of f
  ratio = 2)
# AE thinks state is initial condition and i0 scales everything
fsi = with(pars,
  r/(r+ur))
nsi = 1 - fsi
i0 = 8
state_0 = c(S  = nsi * (pars$N - i0),
  E1 = 0.4 * nsi * i0,
  E2 = 0.1 * nsi * i0,
  I =  0.5 * nsi * i0,
  Q = 0,
  R = 0,
  Sd = fsi * (pars$N - i0),
  E1d = 0.4 * fsi * i0,
  E2d = 0.1 * fsi * i0,
  Id = 0.5 * fsi * i0,
  Qd = 0,
  Rd = 0)

times = seq(from=-30,   # should probably define values up front
  to=30,
  by=0.1)

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
  try(if (min(out$time) > day - (22 + 1)) {
    stop("we need an earlier start time for the model")
  })

  # relevant times to identify new cases
  ii <- which(out$time > day - 22 & out$time <= day) # then just
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

sim_dat <- purrr::map(1:20, function(x) {
  example_simulation = as.data.frame(deSolve::ode(y = state_0,
    times = times,
    func = socdistmodel,
    parms = pars,
    sdtiming = sdtiming_gradual))

  # example_simulation %>% reshape2::melt(id.vars = "time") %>% ggplot(aes(time, value)) + facet_wrap(~variable, scales = "free_y") + geom_line()

  dat <- data.frame(
    Date = seq(lubridate::ymd("2020-03-01"),
      lubridate::ymd("2020-04-01"), by = "day"))
  dat$day <- seq_along(dat$Date)
  lambda_d <- sapply(seq(1, max(example_simulation$time)), function(x) {
    getlambd(example_simulation, pars = pars_default, data = dat, day = x)
  })

  # plot(seq(1, max(example_simulation$time)), lambda_d)

  sim_dat <- data.frame(day = seq(1, max(example_simulation$time)),
    lambda_d = lambda_d, obs = rpois(30, lambda_d))
  sim_dat
})

saveRDS(sim_dat, file = "sim-test-dat.rds")
