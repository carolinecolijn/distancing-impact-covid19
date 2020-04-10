make_projection_plot <- function(models, cumulative = FALSE,
  first_date = "2020-03-01", ylim = c(0, 200), outer_quantile = c(0.025, 0.975),
  facet = TRUE, ncol = 1, cols = NULL) {

  obj <- models[[1]]
  actual_dates <- seq(lubridate::ymd(first_date),
    lubridate::ymd(first_date) + max(obj$days), by = "1 day")

  out <- purrr::map_df(models, function(.x) {
    temp <- .x$post$y_rep %>%
      reshape2::melt() %>%
      dplyr::rename(day = Var2)

    if (cumulative) {
      temp <- temp %>%
        group_by(iterations) %>%
        mutate(value = cumsum(value)) %>%
        ungroup()
    }

    temp %>%
      group_by(day) %>%
      summarise(
        lwr = quantile(value, probs = outer_quantile[1]),
        lwr2 = quantile(value, probs = 0.25),
        upr = quantile(value, probs = outer_quantile[2]),
        upr2 = quantile(value, probs = 0.75),
        med = median(value)
      ) %>%
      mutate(day = actual_dates[day])
  }, .id = "Scenario")

  lambdas <- purrr::map_df(models, function(.x) {
    temp <- .x$post$lambda_d %>%
      reshape2::melt() %>%
      dplyr::rename(day = Var2) %>% as_tibble()

    if (cumulative) {
      temp <- temp %>%
        group_by(iterations) %>%
        mutate(value = cumsum(value)) %>%
        ungroup()
    }

    temp %>%
      group_by(day) %>%
      summarise(
        med = median(value)
      ) %>%
      mutate(day = actual_dates[day])
  }, .id = "Scenario")


  if (cumulative) {
    dat <- tibble(day = actual_dates[1:obj$last_day_obs],
      value = cumsum(obj$daily_cases))
  } else {
    dat <- tibble(day = actual_dates[1:obj$last_day_obs],
      value = obj$daily_cases)
  }
  if (is.null(cols)) {
    cols <- RColorBrewer::brewer.pal(8, "Dark2")
    cols <- rep(cols, 5)
  }
  g <- ggplot(out, aes(x = day, y = med, ymin = lwr, ymax = upr, colour = Scenario,
    fill = Scenario)) +
    geom_ribbon(alpha = 0.2, colour = NA) +
    geom_ribbon(alpha = 0.2, mapping = aes(ymin = lwr2, ymax = upr2), colour = NA) +
    # geom_line(alpha = 0.9, lwd = 1) +
    geom_line(data = lambdas, aes(x = day, y = med, colour = Scenario), alpha = 1, lwd = 1, inherit.aes = FALSE) +
    geom_point(
      data = dat,
      col = "black", inherit.aes = FALSE, aes(x = day, y = value),
    ) +
    geom_line(
      data = dat,
      col = "black", inherit.aes = FALSE, aes(x = day, y = value), lwd = 0.3,
      alpha = 0.8
    ) +
    ylab(if (!cumulative) "Cases" else "Cumulative cases") +
    xlab("Day") +
    xlim(lubridate::ymd("2020-03-01"), lubridate::ymd("2020-06-08")) +
    geom_vline(xintercept = actual_dates[obj$last_day_obs], lty = 2, alpha = 0.6) +
    coord_cartesian(expand = FALSE, ylim = ylim) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    labs(colour = "Projection scenario", fill = "Projection scenario")

  if (facet)
    g <- g + facet_wrap(~Scenario, ncol = ncol) + theme(legend.position = "none")
  g
}
