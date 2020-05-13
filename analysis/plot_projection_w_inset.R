plot_projection_w_inset <- function(proj_dat, obs_dat, obj, ylim = NULL,
  col = "#377EB8") {

  date_lookup <- tibble(
    date = seq(min(obs_dat$date), max(obs_dat$date) + 0, by = "1 day"),
    day = seq_len(max(proj_dat$day)))
  p2 <- left_join(proj_dat, date_lookup) %>%
    select(-day) %>%
    rename(day = date)
  obs2 <- obs_dat %>%
    select(-day) %>%
    rename(day = date)

  # if (is.null(ylim)) ylim <- c(0, max(p2$y_rep_0.95))
  half_line <- 11/2
  g <- covidseir::tidy_seir(p2, resample_y_rep = 50) %>%
    covidseir::plot_projection(obs2, col = col) +
    scale_y_continuous(labels = function(x) x/1) +
    facet_null() +
    ggsidekick::theme_sleek() + ylab("") +
    coord_cartesian(expand = FALSE, xlim = c(lubridate::ymd("2020-03-01"), max(p2$day)), ylim = ylim) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
    theme(axis.title.x.bottom = element_blank(),
      plot.margin =
        margin(t = 5, r = 1.5, b = -8, l = -3),
      axis.title.y = element_text(angle = 90,
        margin = margin(r = 2), vjust = 1, size = 10)
    ) #+
     # ylab("Reported cases (1000s)")

  # g <- g + geom_vline(xintercept = ymd("2020-03-05") + obj$post$start_decline[1:200], alpha = 0.1) +geom_vline(xintercept = ymd("2020-03-05") + obj$post$end_decline[1:200], alpha = 0.1)
  # # g
  #

  # browser()
  # f2_hist

  # plot_with_inset <-
  #   cowplot::ggdraw() +
  #   cowplot::draw_plot(g) #+
    # cowplot::draw_plot(f2_hist, x = inset_x, y = inset_y, width = width, height = height)

  # plot_with_inset
  g
}

f2_plot <- function(obj, threshold, col = "black") {
  .hist_blue <- RColorBrewer::brewer.pal(6, "Blues")[5]
  f2 <- obj$post$f_s[,1]
  .x <- seq(0, 1, length.out = 300)
  breaks <- seq(min(.x), max(.x), 0.020)
  f2_hist <- ggplot(tibble(f2 = f2)) +
    # geom_ribbon(
    #   data = tibble(
    #     f2 = 1 - .x,
    #     density = dbeta(.x, obj$f2_prior_beta_shape1, obj$f2_prior_beta_shape2)
    #   ),
    #   aes(x = f2, ymin = 0, ymax = density), alpha = 0.3, colour = "grey50",
    #   fill = "grey50", size = 0.4
    # ) +
    geom_histogram(
      breaks = breaks, aes(x = f2, y = ..density..),
      fill = col, alpha = 1, colour = "grey90", lwd = 0.1
    ) +
    ylab("Density") +
    coord_cartesian(xlim = c(0, .53), expand = FALSE) +
    xlab("") +
    # xlab("1 - f2") +
    scale_x_continuous(breaks = seq(0, .5, 0.1), labels = c("0", "", "0.2", "", "0.4", "")) +
    geom_vline(xintercept = threshold, lty = 1, col = "grey50") +
    ggsidekick::theme_sleek() +
    theme(axis.line.y = element_blank(), panel.border = element_blank(),
      axis.text.y = element_blank(), axis.line.x = element_line(colour = "grey75"),
      axis.ticks.y = element_blank(), axis.title.y = element_blank(),
      axis.title.x.bottom = element_text(size = 9),
      axis.text.x = element_text(size = 8),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA))
  f2_hist
}
