get_thresh_covidseir <- function(obj, iter = 1:25, forecast_days = 60,
  fs = seq(0.4, 0.95, length.out = 6),
  show_plot = TRUE,
  window_check = 25) {
  m_fs <- purrr::map(fs, function(.f) {
    cat("Projecting", round(.f, 2), "\n")
    project_seir(obj, forecast_days = forecast_days, iter = iter,
      f_fixed_start = nrow(obj$daily_cases) + 1,
      f_fixed = rep(.f, forecast_days),
      return_states = TRUE)
  })
  slopes <- purrr::map2_df(m_fs, fs, function(x, y) {
    temp <- x %>%
      dplyr::filter(time > max(x$time) - window_check,
        variable %in% c("I", "Id")) %>%
      group_by(.iteration, time) %>%
      summarize(
        I = value[variable == "I"], Id = value[variable == "Id"],
        prevalence = I + Id
      )
    iters <- temp %>%
      group_by(.iteration) %>%
      summarise(iter = .iteration[[1]])
    temp %>%
      group_by(.iteration) %>%
      group_split() %>%
      purrr::map(~ lm(log(prevalence) ~ time, data = .x)) %>%
      purrr::map_df(~ tibble(slope = coef(.x)[[2]])) %>%
      mutate(f = y) %>%
      ungroup() %>%
      mutate(.iteration = iters$iter)
  })
  if (show_plot) {
    plot(slopes$f, slopes$slope)
  }
  mlm <- lm(slope ~ f, data = slopes)
  nd <- data.frame(f = seq(0.2, 0.9, length.out = 5000))
  nd$predicted_slope <- stats::predict(mlm, newdata = nd)
  dplyr::filter(nd, predicted_slope > 0) %>% `[`(1, "f")
}
