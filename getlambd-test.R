getlambd <- function(out,
                     pars,
                     day,
                     data = bcdata,
                     sampFrac = 0.35,
                     delayShape = 1.9720199,
                     delayScale = 12.0529283) {
  # 10.685 with parameters 1.972... and 12.0529...
  meanDelay <- delayScale * gamma(1 + 1 / delayShape)

  # browser()
  # relevant times to identify new cases
  ii <- which(out$time > day - 2 * meanDelay & out$time <= day) # then just
  # need dweibull over this range
  dx <- out$time[ii[2]] - out$time[ii[1]] # assumes equal time intervals

  # all new cases arising at each of those times
  incoming <- pars$k2 * (out$E2[ii] + out$E2d[ii])

  march15_modelform <- data$day[which(data$Date == as.Date("2020-03-14"))]
  thisSamp <- ifelse(day < march15_modelform,
    sampFrac,
    sampFrac * pars$ratio
  )

  plot(max(out$time[ii]) - out$time[ii],
    dweibull(x = max(out$time[ii]) - out$time[ii],
    shape = delayShape, scale = delayScale))
  # each of the past times' contribution to this day's case count
  ft <- thisSamp * incoming * dweibull(
    x = max(out$time[ii]) - out$time[ii], # values over which to integrate?
    shape = delayShape,
    scale = delayScale
  )

  # return numerical integral of ft
  # trapezoid rule for integrating?
  0.5 * (dx) * (ft[1] + 2 * sum(ft[2:(length(ft) - 1)]) + ft[length(ft)])
}

load("out.rda")
pars <- list(
  N = 4400000, D = 5, R0 = 2.5, k1 = 0.25, k2 = 1, q = 0.05,
  r = 1, ur = 0.8, f = 1, f2 = 0, ratio = 2
)
load("bcdata.rda")

xx <- purrr::map_dbl(bcdata$day, ~getlambd(out, pars = pars, day = ., data = bcdata))

sum(xx)
sum(bcdata$Diffs)
