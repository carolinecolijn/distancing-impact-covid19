source(here::here("analysis/data-model-prep.R"))

# m <- fit_seeiqr(daily_diffs, seeiqr_model = seeiqr_model, iter = 2000, chains = 8)
# print(m$fit, pars = c("R0", "f2", "phi"))
# saveRDS(m, file = "data-generated/main-fit-2000.rds")
m <- readRDS("data-generated/main-fit-2000.rds")

.last_day <- m$last_day_obs
.last_day
.f2_now <- round(mean(m$post$f2), 2)
write_tex(100 * (1 - .f2_now), "currentPercInCycles")
write_tex((1 - .f2_now), "currentFracInCycles")

sdtiming_cycle_4x4 <- function(
                               t, start_decline = 15, end_decline = 22, last_obs = .last_day,
                               f_vec = c(rep(NA, last_obs), rep(c(rep(0.8, 7 * 4), rep(.f2_now, 7 * 4)), 12)),
                               f1 = pars$f1,
                               f2 = pars$f2) {
  if (t < start_decline) {
    return(f1)
  }
  if (t >= start_decline & t < end_decline) {
    return(f2 + (end_decline - t) * (f1 - f2) / (end_decline - start_decline))
  }
  floor_t <- floor(t)
  if (t >= end_decline & floor_t <= last_obs) {
    return(f2)
  }
  if (t >= end_decline & floor_t > last_obs) {
    return(f_vec[floor_t])
  }
}

sdtiming_cycle_3x3 <- sdtiming_cycle_4x4
formals(sdtiming_cycle_3x3)$f_vec <-
  c(rep(NA, .last_day), rep(c(rep(0.8, 7 * 3), rep(.f2_now, 7 * 3)), 12))

plan(multisession, workers = parallel::detectCores() / 2)

proj_days <- .last_day + 4 * 7 * 4
.times <- seq(-30, proj_days, 0.1)
pred_4x4 <- list(m$post$R0, m$post$f2, m$post$phi, seq_along(m$post$R0)) %>%
  furrr::future_pmap_dfr(reproject_fits,
    obj = m, .sdfunc = sdtiming_cycle_4x4,
    .time = .times, .progress = TRUE,
    .options = furrr::future_options(
      globals =
        c(".last_day", "m", "sdtiming_cycle_4x4", "socdistmodel", "getlambd", ".f2_now")
    )
  )
saveRDS(pred_4x4, file = "data-generated/pred_4x4.rds")
pred_4x4 <- readRDS("data-generated/pred_4x4.rds")

# re-sample obs. model for smoother plots:
pred_4x4 <- purrr::map_dfr(1:10, function(i) {
  pred_4x4$y_rep <- MASS::rnegbin(length(pred_4x4$y_rep),
    pred_4x4$lambda_d,
    theta = pred_4x4$phi
  )
  pred_4x4
})

pred_3x3 <- list(m$post$R0, m$post$f2, m$post$phi, seq_along(m$post$R0)) %>%
  furrr::future_pmap_dfr(reproject_fits,
    obj = m, .sdfunc = sdtiming_cycle_3x3,
    .time = .times, .progress = TRUE,
    .options = furrr::future_options(
      globals =
        c(".last_day", "m", "sdtiming_cycle_3x3", "socdistmodel", "getlambd", ".f2_now")
    )
  )
saveRDS(pred_3x3, file = "data-generated/pred_3x3.rds")
pred_3x3 <- readRDS("data-generated/pred_3x3.rds")

# re-sample obs. model for smoother plots:
pred_3x3 <- purrr::map_dfr(1:10, function(i) {
  pred_3x3$y_rep <- MASS::rnegbin(length(pred_3x3$y_rep),
    pred_3x3$lambda_d,
    theta = pred_3x3$phi
  )
  pred_3x3
})

plan(sequential)

prep_dat <- function(.dat, Scenario = "") {
  actual_dates <- seq(dat$Date[1], dat$Date[1] + proj_days, by = "1 day")
  outer_quantile <- c(0.05, 0.95)
  y_rep <- .dat %>%
    mutate(value = y_rep) %>%
    group_by(day) %>%
    summarise(
      lwr = quantile(value, probs = outer_quantile[1]),
      lwr2 = quantile(value, probs = 0.25),
      upr = quantile(value, probs = outer_quantile[2]),
      upr2 = quantile(value, probs = 0.75),
      med = median(value)
    ) %>%
    mutate(day = actual_dates[day], Scenario = Scenario)
  lambdas <- .dat %>%
    group_by(day) %>%
    mutate(value = lambda_d) %>%
    summarise(
      med = median(value)
    ) %>%
    mutate(day = actual_dates[day], Scenario = Scenario)
  list(y_rep = y_rep, mu = lambdas)
}

x <- prep_dat(pred_4x4)
.max <- max(x$y_rep$upr) * 1.04
g1 <- make_projection_plot(
  models = list(m), mu_dat = x$mu,
  y_rep_dat = x$y_rep, ylim = c(0, .max), points_size = 1.25
) +
  theme(plot.margin = margin(11 / 2, 11, 11 / 2, 11 / 2))
for (i in seq(1, 4, 2)) {
  .inc <- 7 * 4
  g_last_day <- dat$Date[1] + .last_day
  g1 <- g1 + annotate("rect",
    xmin = g_last_day + (i - 1) * .inc - 1,
    xmax = g_last_day + i * .inc - 1,
    ymin = 0, ymax = Inf, fill = "#00000012"
  )
}

x <- prep_dat(pred_3x3)
g2 <- make_projection_plot(
  models = list(m), mu_dat = x$mu,
  y_rep_dat = x$y_rep, ylim = c(0, .max), points_size = 1.25
) +
  theme(plot.margin = margin(11 / 2, 11, 11 / 2, 11 / 2))
for (i in seq(1, 6, 2)) {
  .inc <- 7 * 3
  g_last_day <- dat$Date[1] + .last_day
  g2 <- g2 + annotate("rect",
    xmin = g_last_day + (i - 1) * .inc - 1,
    xmax = g_last_day + i * .inc - 1,
    ymin = 0, ymax = Inf, fill = "#00000012"
  )
}

cowplot::plot_grid(g1, g2, ncol = 1, labels = "AUTO", label_x = 0.125, label_y = 0.96)
ggsave("figs-ms/f2-cycling.png", width = 4.5, height = 5, dpi = 400)
ggsave("figs-ms/f2-cycling.pdf", width = 4.5, height = 5)

