make_quick_plots <- function(obj, id = "", ext = ".pdf", first_date = "2020-03-01") {
  post <- obj$post
  fit <- obj$fit

  actual_dates <- seq(lubridate::ymd(first_date),
    lubridate::ymd(first_date) + max(obj$days), by = "1 day")

  fit_array <- as.array(fit)
  if ("phi" %in% names(post)) {
    g <- bayesplot::mcmc_trace(fit_array, pars = c("R0", "f2", "phi[1]"))
  } else {
    g <- bayesplot::mcmc_trace(fit_array, pars = c("R0", "f2"))
  }
  ggsave(paste0("figs/traceplot", id, ext), width = 6, height = 3)

  R0 <- post$R0
  .x <- seq(2, 3.2, length.out = 200)
  breaks <- seq(min(.x), max(.x), 0.02)
  g1 <- ggplot(tibble(R0 = R0)) +
    geom_ribbon(
      data = tibble(R0 = .x,
        density = dlnorm(.x, obj$R0_prior[1], obj$R0_prior[2])),
      aes(x = R0, ymin = 0, ymax = density), alpha = 0.5, colour = "grey50",
      fill = "grey50"
    ) +
    ylab("Probability density") +
    geom_histogram(
      breaks = breaks, aes(x = R0, y = ..density..),
      fill = "blue", alpha = 0.5
    ) +
    coord_cartesian(xlim = range(.x), expand = FALSE)

  f2 <- post$f2
  .x <- seq(0, 1, length.out = 200)
  breaks <- seq(min(.x), max(.x), 0.03)
  g2 <- ggplot(tibble(f2 = f2)) +
    geom_ribbon(
      data = tibble(f2 = .x,
        density = dbeta(.x, obj$f2_prior_beta_shape1, obj$f2_prior_beta_shape2)),
      aes(x = f2, ymin = 0, ymax = density), alpha = 0.5, colour = "grey50",
      fill = "grey50"
    ) +
    geom_histogram(
      breaks = breaks, aes(x = f2, y = ..density..),
      fill = "blue", alpha = 0.5
    ) +
    ylab("Probability density") +
    coord_cartesian(xlim = range(.x), expand = FALSE) +
    xlab("Social distancing effect") +
    scale_x_continuous(breaks = seq(0, 1, 0.2))

  if (obj$stan_data$n_sampFrac2 == 1) {
    sampFrac2 <- post$sampFrac2
    .x <- seq(0, 1, length.out = 200)
    breaks <- seq(min(.x), max(.x), 0.03)
    g5 <- ggplot(tibble(sampFrac2 = sampFrac2)) +
      geom_ribbon(
        data = tibble(sampFrac2 = .x,
          density = dbeta(.x, obj$stan_data$sampFrac2_prior[1], obj$stan_data$sampFrac2_prior[2])),
        aes(x = sampFrac2, ymin = 0, ymax = density), alpha = 0.5, colour = "grey50",
        fill = "grey50"
      ) +
      geom_histogram(
        breaks = breaks, aes(x = sampFrac2, y = ..density..),
        fill = "blue", alpha = 0.5
      ) +
      ylab("Probability density") +
      coord_cartesian(xlim = range(.x), expand = FALSE) +
      xlab("Fraction sampled") +
      scale_x_continuous(breaks = seq(0, 1, 0.2))
    ggsave(paste0("figs/sampFrac2", id, ext), width = 6, height = 4)
  }

  if ("phi" %in% names(post)) {
    phi_hat <- post$phi[, 1]
    .x <- seq(0.001, 100, length.out = 20000)
    breaks <- seq(0, 9, 0.2)
    g3 <- ggplot(tibble(phi = phi_hat)) +
      geom_ribbon(
        data = tibble(phi = 1/sqrt(.x),
          density = dnorm(.x, 0, obj$phi_prior)),
        aes(x = phi, ymin = 0, ymax = density),
        alpha = 0.5, colour = "grey50", fill = "grey50"
      ) +
      ylab("Probability density") +
      geom_histogram(
        breaks = breaks, aes(x = phi, y = ..density..),
        fill = "blue", alpha = 0.5
      ) +
      coord_cartesian(expand = FALSE, xlim = c(0, 9))
    # ggsave(paste0("figs/phi", id, ext), width = 6, height = 4)
    cowplot::plot_grid(g1, g2, ncol = 1)
    ggsave(paste0("figs/theta", id, ext), width = 4, height = 5.5)
  } else {
    cowplot::plot_grid(g1, g2, ncol = 1)
    ggsave(paste0("figs/theta", id, ext), width = 4, height = 5.5)
  }

  draws <- sample(seq_along(post$lambda_d[, 1]), 100L)
  variables_df <- dplyr::tibble(
    variable = names(obj$state_0),
    variable_num = seq_along(obj$state_0)
  )

  if ("y_hat" %in% names(post)) {
    ts_df <- dplyr::tibble(time = obj$time, time_num = seq_along(obj$time))
    states <- reshape2::melt(post$y_hat) %>%
      dplyr::rename(time_num = Var2, variable_num = Var3) %>%
      dplyr::filter(iterations %in% draws) %>%
      dplyr::left_join(variables_df, by = "variable_num") %>%
      dplyr::left_join(ts_df, by = "time_num")

    g <- ggplot(states, aes(time, value, group = iterations)) +
      geom_line(alpha = 0.1) +
      facet_wrap(~variable, scales = "free_y") +
      geom_vline(xintercept = obj$last_day_obs, lty = 2, alpha = 0.6) +
      scale_x_continuous(breaks = seq(-30, 150, 10))
    ggsave(paste0("figs/states", id, ext), width = 12, height = 7.5)
  }

  draws <- sample(seq_along(post$lambda_d[, 1]), 100L)
  g <- reshape2::melt(post$lambda_d) %>%
    dplyr::rename(day = Var2) %>%
    dplyr::filter(iterations %in% draws) %>%
    ggplot(aes(day, value, group = iterations)) +
    geom_line(alpha = 0.05) +
    geom_point(
      data = tibble(day = seq_along(obj$daily_cases), value = obj$daily_cases),
      inherit.aes = FALSE, aes(x = day, y = value)
    )
  ggsave(paste0("figs/expected-case-diffs", id, ext), width = 6, height = 4)

  # Posterior predictive checks:

  draws <- sample(seq_along(post$y_rep[, 1]), 50L)
  g <- post$y_rep %>%
    reshape2::melt() %>%
    dplyr::filter(iterations %in% draws) %>%
    dplyr::rename(day = Var2) %>%
    ggplot(aes(day, value, group = iterations)) +
    geom_line(alpha = 0.1) +
    geom_line(
      data = tibble(day = seq_len(obj$last_day_obs), value = obj$daily_cases),
      col = "red", inherit.aes = FALSE, aes(x = day, y = value)
    )
  ggsave(paste0("figs/posterior-predictive-case-diffs", id, ext), width = 6, height = 4)

  set.seed(1929)
  draws <- sample(seq_along(post$y_rep[, 1]), 24L)
  g <- post$y_rep %>%
    reshape2::melt() %>%
    dplyr::filter(iterations %in% draws) %>%
    dplyr::rename(day = Var2) %>%
    mutate(Type = "Posterior\npredictive\nsimulation") %>%
    bind_rows(tibble(
      iterations = 0, day = seq_len(obj$last_day_obs),
      value = obj$daily_cases, Type = "Observed"
    )) %>%
    ggplot(aes(day, value, colour = Type)) +
    geom_line(lwd = 0.7) +
    facet_wrap(vars(iterations)) +
    ylab("New cases") +
    xlab("Day") +
    scale_color_manual(values = c("red", "grey40")) +
    geom_vline(xintercept = obj$last_day_obs, lty = 2, alpha = 0.6)
  ggsave(paste0("figs/posterior-predictive-case-diffs-facet", id, ext), width = 9, height = 6.25)

  dat <- tibble(day = actual_dates[1:obj$last_day_obs], value = obj$daily_cases)
  g <- post$y_rep %>%
    reshape2::melt() %>%
    dplyr::rename(day = Var2) %>%
    dplyr::group_by(day) %>%
    summarise(
      lwr = quantile(value, probs = 0.1),
      lwr2 = quantile(value, probs = 0.25),
      upr = quantile(value, probs = 0.9),
      upr2 = quantile(value, probs = 0.75),
      med = median(value)
    ) %>%
    mutate(day = actual_dates[day]) %>%
    ggplot(aes(day, y = med, ymin = lwr, ymax = upr)) +
    geom_ribbon(alpha = 0.2) +
    geom_ribbon(alpha = 0.2, mapping = aes(ymin = lwr2, ymax = upr2)) +
    geom_line(alpha = 0.9, lwd = 1) +
    geom_point(
      data = dat,
      col = "red", inherit.aes = FALSE, aes(x = day, y = value),
    ) +
    geom_line(
      data = dat,
      col = "red", inherit.aes = FALSE, aes(x = day, y = value), lwd = 0.2, alpha = 0.5
    ) +
    ylab("New cases") +
    xlab("Day") +
    geom_vline(xintercept = actual_dates[obj$last_day_obs], lty = 2, alpha = 0.6) +
    coord_cartesian(expand = FALSE) +
    theme(plot.margin = margin(11/2,11, 11/2, 11/2))
  ggsave(paste0("figs/posterior-predictive-quantiles-case-diffs", id, ext), width = 6, height = 4)

  # cumulative

  dat <- tibble(day = actual_dates[1:obj$last_day_obs], value = cumsum(obj$daily_cases))

  g <- post$y_rep %>%
    reshape2::melt() %>%
    dplyr::rename(day = Var2) %>%
    dplyr::group_by(iterations) %>%
    mutate(value = cumsum(value)) %>%
    dplyr::group_by(day) %>%
    summarise(
      lwr = quantile(value, probs = 0.1),
      lwr2 = quantile(value, probs = 0.25),
      upr = quantile(value, probs = 0.9),
      upr2 = quantile(value, probs = 0.75),
      med = median(value)
    ) %>%
    mutate(day = actual_dates[day]) %>%
    ggplot(aes(day, y = med, ymin = lwr, ymax = upr)) +
    geom_ribbon(alpha = 0.2) +
    geom_ribbon(alpha = 0.2, mapping = aes(ymin = lwr2, ymax = upr2)) +
    geom_line(alpha = 0.9, lwd = 1) +
    geom_point(
      data = dat,
      col = "red", inherit.aes = FALSE, aes(x = day, y = value),
    ) +
    geom_line(
      data = dat,
      col = "red", inherit.aes = FALSE, aes(x = day, y = value), lwd = 0.2, alpha = 0.5
    ) +
    ylab("Cumulative cases") +
    xlab("Day") +
    geom_vline(xintercept = actual_dates[obj$last_day_obs], lty = 2, alpha = 0.6) +
    coord_cartesian(expand = FALSE) +
    theme(plot.margin = margin(11/2,11, 11/2, 11/2))
  ggsave(paste0("figs/posterior-predictive-quantiles-case-cumsum", id, ext), width = 6, height = 4)

}
