make_projection_plot <- function(models, cumulative = FALSE,
  first_date = "2020-03-01", ylim = c(0, max(out$upr) * 1.03), outer_quantile = c(0.05, 0.95),
  facet = TRUE, ncol = 1, cols = NULL, linetype = c("mu", "obs"),
  omitted_days = NULL, y_rep_dat = NULL, mu_dat = NULL, points_size = 1.25,
  sc_order = NULL) {

  linetype <- match.arg(linetype)
  obj <- models[[1]]
  actual_dates <- seq(lubridate::ymd(first_date),
    lubridate::ymd(first_date) + max(obj$days), by = "1 day")

  if (is.null(y_rep_dat) || is.null(mu_dat)) {
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
  } else {
    out <- y_rep_dat
    lambdas <- mu_dat
  }

  if (cumulative) {
    dat <- tibble(day = actual_dates[1:obj$last_day_obs],
      value = cumsum(obj$daily_cases))
  } else {
    dat <- tibble(day = actual_dates[1:obj$last_day_obs],
      value = obj$daily_cases)
  }
  if (is.null(cols)) {
    # cols <- RColorBrewer::brewer.pal(8, "Dark2")
    # cols <- rep(cols, 5)
    cols <- rep("#3182BD", 99)
  }

  if (!is.null(sc_order)) {
    out$Scenario <- factor(out$Scenario, levels = sc_order)
    lambdas$Scenario <- factor(lambdas$Scenario, levels = sc_order)
  }
  g <- ggplot(out, aes(x = day, y = med, ymin = lwr, ymax = upr, colour = Scenario,
    fill = Scenario)) +
    annotate("rect", xmin = actual_dates[obj$last_day_obs], xmax = max(out$day), ymin = 0, ymax = ylim[2], fill = "grey95") +
    coord_cartesian(expand = FALSE, ylim = ylim, xlim = range(out$day)) +
    geom_ribbon(alpha = 0.2, colour = NA) +
    geom_ribbon(alpha = 0.2, mapping = aes(ymin = lwr2, ymax = upr2), colour = NA)

  if (linetype == "obs")
    g <- g + geom_line(alpha = 1, lwd = 1)
  if (linetype == "mu")
    g <- g + geom_line(data = lambdas, aes(x = day, y = med, colour = Scenario), alpha = 1, lwd = 1, inherit.aes = FALSE)

  g <- g +
    geom_line(
      data = dat,
      col = "black", inherit.aes = FALSE, aes(x = day, y = value), lwd = 0.35,
      alpha = 0.9
    )

  if (!is.null(omitted_days)) {
    g <- g +
      geom_point(
        data = dat[omitted_days,,drop=FALSE],
        col = "grey30", inherit.aes = FALSE, aes(x = day, y = value), pch = 4, fill = "grey95",  size = points_size
      ) +
      geom_point(
        data = dat[-omitted_days, ,drop=FALSE],
        col = "grey30", inherit.aes = FALSE, aes(x = day, y = value), pch = 21, fill = "grey95",  size = points_size
      )
  } else {
    g <- g + geom_point(
        data = dat,
        col = "grey30", inherit.aes = FALSE, aes(x = day, y = value), pch = 21, fill = "grey95", size = points_size
      )
  }

  g <- g +
    ylab(if (!cumulative) "Reported cases" else "Cumulative reported cases") +
    xlab("Day") +
    # xlim(lubridate::ymd("2020-03-01"), lubridate::ymd("2020-06-08")) +
    # geom_vline(xintercept = actual_dates[obj$last_day_obs], lty = 2, alpha = 0.6) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    labs(colour = "Projection scenario", fill = "Projection scenario") +
    theme(axis.title.x = element_blank())

  if (facet && length(unique(out$Scenario)) > 1)
    g <- g + facet_wrap(~Scenario, ncol = ncol) + theme(legend.position = "none")

  if (length(unique(out$Scenario)) == 1) {
    g <- g + guides(fill = FALSE, colour = FALSE)
  }
  g
}
