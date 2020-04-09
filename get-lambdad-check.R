getlambd_stan <- function(time, E2, E2d, time_day_id0, time_day_id, sampFrac,
                          .T = 30, k2 = 1, delayScale = 9,
                          delayShape = 1.9720199) {
  meanDelay <- delayScale * tgamma(1 + 1 / delayShape)
  dx <- time[2] - time[1]
  for (t in 1:.T) {
    ft[t] <- 0
  }
  for (n in 1:N) {
    for (t in time_day_id0[n]:time_day_id[n]) {
      ft[t] <- sampFrac[t] * k2 * (E2 + E2d) *
        exp(dweibull(time[time_day_id[n]] - time[t], delayShape, delayScale, log = TRUE))
    }
    sum_ft_inner <- 0
    for (t in (time_day_id0[n] + 1):(time_day_id[n] - 1)) {
      sum_ft_inner <- sum_ft_inner + ft[t]
    }
    lambda_d[n] <- 0.5 * dx * (ft[time_day_id0[n]] +  2 * sum_ft_inner + ft[time_day_id[n]])
  }
  lambda_d
}

getlambd_R <- function(time,
  E2, E2d, k2 = 1, sampFrac,
                       pars,
                       day,
                       data = bcdata,
                       sampFrac = 0.2,
                       delayShape = 1.9720199,
                       delayScale = 9) {
  meanDelay <- delayScale * gamma(1 + 1 / delayShape)
  ii <- which(out$time > -999 & out$time <= day)
  dx <- time[2] - time[1]
  incoming <- k2 * (E2 + E2d)
  ft <- thisSamp * incoming * dweibull(
    x = max(out$time[ii]) - out$time[ii],
    shape = delayShape,
    scale = delayScale
  )
  0.5 * (dx) * (ft[1] + 2 * sum(ft[2:(length(ft) - 1)]) + ft[length(ft)])
}
