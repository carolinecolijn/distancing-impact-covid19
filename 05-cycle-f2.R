library(future)
plan(multisession)
source("data-model-prep.R")
m <- fit_seeiqr(daily_diffs, iter = 1000, chains = 8)
print(m$fit, pars = c("R0", "f2", "phi"))

.last_day <- m$last_day_obs

sdtiming_cycle_4x4 <- function(
  t, start_decline = 15, end_decline = 22,
  f_vec = c(rep(NA, .last_day), rep(c(rep(0.8, 7 * 4), rep(0.35, 7 * 4)), 12)),
  f1 = pars$f1,
  f2 = pars$f2) {
  if (t < start_decline) {
    return(f1)
  }
  if (t >= start_decline & t < end_decline) {
    return(f2 + (end_decline - t) * (f1 - f2) / (end_decline - start_decline))
  }
  if (t >= end_decline & floor(t) <= 42) {
    return(f2)
  }
  if (t >= end_decline & floor(t) > 42) {
    return(f_vec[floor(t)])
  }
}

sdtiming_cycle_3x3 <- sdtiming_cycle_4x4
formals(sdtiming_cycle_3x3)$f_vec <-
  c(rep(NA, .last_day), rep(c(rep(0.8, 7 * 4), rep(0.35, 7 * 4)), 12))

.times <- seq(-30, 160, 0.1)
pred_4x4 <- list(m$post$R0, m$post$f2, m$post$phi, seq_along(m$post$R0)) %>%
  furrr::future_pmap_dfr(reproject_fits, obj = m, .sdfunc = sdtiming_cycle,
    .time = .times)

pred_3x3 <- list(m$post$R0, m$post$f2, m$post$phi, seq_along(m$post$R0)) %>%
  furrr::future_pmap_dfr(reproject_fits, obj = m, .sdfunc = sdtiming_cycle,
    .time = .times)

pred %>%
  group_by(day) %>%
  summarise(
    lwr = quantile(y_rep, probs = 0.05),
    lwr2 = quantile(y_rep, probs = 0.25),
    upr = quantile(y_rep, probs = 0.95),
    upr2 = quantile(y_rep, probs = 0.75),
    med = median(y_rep)
  ) %>%
  ggplot(aes(day, med)) +
  geom_vline(xintercept = 42) +
  annotate("rect", xmin = 42, xmax = 42 + 7 * 4,
    ymin = -20, ymax = 9000, fill = "#00000010") +
  annotate("rect", xmin = 42 + 7 * 8, xmax = 42 + 7 * 8 + 7 * 4,
    ymin = -20, ymax = 9000, fill = "#00000010") +
  coord_cartesian(expand = FALSE, ylim = c(0, 150)) +
  geom_ribbon(alpha = 0.2, mapping = aes(ymin = lwr, ymax = upr), colour = NA) +
  geom_line()

plan(sequential)
