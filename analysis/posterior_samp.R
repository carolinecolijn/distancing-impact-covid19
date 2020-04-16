posterior_samp <- function(model, samps_per_iteration) {
  m <- model
  Rcpp::sourceCpp('get_lambd.cpp')
  out <- purrr::map_df(seq_along(m$post$R0), function(i) {
    lambd <- get_lambd_cpp(
      time = m$time,
      k2 = m$stan_data$x_r[['k2']],
      E2 = m$post$y_hat[i,,3],
      E2d = m$post$y_hat[i,,9],
      delayShape = m$stan_data$delayShape,
      delayScale = m$stan_data$delayScale,
      N = max(m$time),
      T = length(m$time),
      sampFrac = m$stan_data$sampFrac,
      time_day_id0 = m$stan_data$time_day_id0 - 1,
      time_day_id = m$stan_data$time_day_id - 1)
    tibble(iterations = i, day = 1:max(m$time), lambd = lambd, phi = m$post$phi[i])
  })
  out2 <- purrr::map_df(seq_len(samps_per_iteration), function(i) {
    temp <- out
    temp$y_rep <- rnbinom(nrow(temp), mu = temp$lambd, size = temp$phi)
    temp$sub_iter <- i
    temp
  })
  # ggplot(out2, aes(day, y_rep, group = paste(iterations, sub_iter))) + geom_line(alpha = 0.01)
  out2$it <- paste(out2$sub_iter, out2$iterations, sep = " - ")
  out2
}
