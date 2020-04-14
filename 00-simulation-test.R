setwd(here::here("selfIsolationModel/stan"))
source("data-model-prep.R")

pars <- list(
  N = 4.4e6, # population of BC
  D = 5,
  R0 = 2.6,
  k1 = 1 / 5,
  k2 = 1,
  q = 0.05,
  r = 1,
  ur = 0.2,
  f1 = 1.0,
  f2 = 0.4,
  ratio = 0.3/0.1 # 2nd stage sampFrac
)
fsi <- with(
  pars,
  r / (r + ur)
)
nsi <- 1 - fsi
i0 <- 8
state_0 <- c(
  S = nsi * (pars$N - i0),
  E1 = 0.4 * nsi * i0,
  E2 = 0.1 * nsi * i0,
  I = 0.5 * nsi * i0,
  Q = 0,
  R = 0,
  Sd = fsi * (pars$N - i0),
  E1d = 0.4 * fsi * i0,
  E2d = 0.1 * fsi * i0,
  Id = 0.5 * fsi * i0,
  Qd = 0,
  Rd = 0
)
times <- seq(
  from = -30,
  to = 45,
  by = 0.1
)
set.seed(128284)

sim_dat <- purrr::map(1:16, function(x) {
  example_simulation <- as.data.frame(deSolve::ode(
    y = state_0,
    times = times,
    func = socdistmodel,
    parms = pars,
    sdtiming = sdtiming_gradual
  ))

  # states <- reshape2::melt(example_simulation, id.vars = "time")
  # ggplot(states, aes(time, value)) +
  #   geom_line() +
  #   facet_wrap(~variable, scales = "free_y")

  dat <- data.frame(
    Date = seq(lubridate::ymd("2020-03-01"),
      lubridate::ymd("2020-03-01") + max(times) + 1,
      by = "day"
    )
  )
  dat$day <- seq_along(dat$Date)
  lambda_d <- sapply(seq(1, max(times)), function(x) {
    getlambd(example_simulation, pars = pars, data = dat, day = x)
  })
  # plot(seq(1, max(example_simulation$time)), lambda_d)
  sim_dat <- data.frame(
    day = seq(1, max(times)),
    lambda_d = lambda_d,
    obs = MASS::rnegbin(max(times), lambda_d, theta = 5)
    # obs = rpois(max(times), lambda_d)
  )
  sim_dat
})

plot(sim_dat[[1]]$lambda_d)
plot(sim_dat[[1]]$obs)
plot(sim_dat[[2]]$obs)
plot(sim_dat[[3]]$obs)

source("fit_seeiqr.R")
library(future)
library(rstan)
library(dplyr)
library(ggplot2)
seeiqr_model <- stan_model("seeiqr.stan")
plan(multisession, workers = parallel::detectCores() / 2)
sim <- furrr::future_map(seq_along(sim_dat), function(x) {
  fit_seeiqr(
    daily_cases = sim_dat[[x]]$obs,
    seeiqr_model = seeiqr_model,
    forecast_days = 1,
    # sampFrac2_prior = c(0.7, 0.1),
    sampFrac2_type = "fixed",
    sampled_fraction1 = 0.1,
    sampled_fraction2 = 0.3,
    iter = 800, # obs_model = "Poisson",
    chains = 1, cores = 1
  )
})
plan(sequential)
saveRDS(sim, "data-generated/sim-fits1.rds")
sim <- readRDS("data-generated/sim-fits1.rds")

purrr::walk(sim, ~ print(.x$fit, pars = c("R0", "f2", "phi", "sampFrac2")))

# Compare posterior predictions to truth: -------------------------------------

out <- purrr::map_df(sim, function(.x) {
  temp <- .x$post$y_rep %>%
    reshape2::melt() %>%
    dplyr::rename(day = Var2)
  temp <- temp %>%
    group_by(day) %>%
    summarise(
      lwr2 = quantile(value, probs = 0.75),
      upr2 = quantile(value, probs = 0.25),
      lwr = quantile(value, probs = 0.95),
      upr = quantile(value, probs = 0.05),
      med = median(value)
    )
  temp
}, .id = "simulation") %>%
  mutate(simulation = sprintf("%02d", as.numeric(simulation)))

out_lambd <- purrr::map_df(sim, function(.x) {
  temp <- .x$post$lambda_d %>%
    reshape2::melt() %>%
    dplyr::rename(day = Var2)
  temp <- temp %>%
    group_by(day) %>%
    summarise(
      lwr2 = quantile(value, probs = 0.75),
      upr2 = quantile(value, probs = 0.25),
      lwr = quantile(value, probs = 0.95),
      upr = quantile(value, probs = 0.05),
      med = median(value)
    )
  temp
}, .id = "simulation") %>%
  mutate(simulation = sprintf("%02d", as.numeric(simulation)))

sim_dat_all <- bind_rows(sim_dat, .id = "simulation") %>% as_tibble() %>%
  mutate(simulation = sprintf("%02d", as.numeric(simulation)))

ggplot(out, aes(x = day, y = med, ymin = lwr2, ymax = upr2)) +
  geom_ribbon(alpha = 0.2, colour = NA) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, colour = NA) +
  geom_line(data = out_lambd, alpha = 0.9, lwd = 0.7) +
  geom_point(
    data = sim_dat_all, pch = 21, size = 1,
    col = "black", inherit.aes = FALSE, aes(x = day, y = obs),
  ) +
  facet_wrap(~simulation, ncol = 4) +
  geom_line(
    data = sim_dat_all,
    col = "black", inherit.aes = FALSE, aes(x = day, y = lambda_d), lwd = 0.5,
    alpha = 1, lty = 2) + ylab("Simulated reported cases") + xlab("Day")
ggsave("figs-ms/sim-ts-ppd.png", width = 8, height = 6.5)

# Compare expectations to truth: ----------------------------------------------

out <- purrr::map_df(sim, function(.x) {
  temp <- .x$post$lambda_d %>%
    reshape2::melt() %>%
    dplyr::rename(day = Var2)
  temp <- temp %>%
    group_by(day) %>%
    summarise(
      lwr2 = quantile(value, probs = 0.75),
      upr2 = quantile(value, probs = 0.25),
      med = median(value)
    )
  temp
}, .id = "simulation")

ggplot(out, aes(x = day, y = med, ymin = lwr2, ymax = upr2)) +
  geom_ribbon(alpha = 0.2, colour = NA) +
  geom_line(alpha = 0.9, lwd = 1) +
  # geom_point(
  #   data = sim_dat[[1]],
  #   col = "black", inherit.aes = FALSE, aes(x = day, y = obs),
  # ) +
  facet_wrap(~simulation, ncol = 4) +
  geom_line(
    data = sim_dat[[1]],
    col = "black", inherit.aes = FALSE, aes(x = day, y = lambda_d), lwd = 0.3,
    alpha = 0.8)
ggsave("figs-ms/sim-ts-hat.png", width = 9, height = 5)

check_sim_theta <- function(.par) {
  out <- purrr::map_df(seq_along(sim), function(x) {
    data.frame(sim = x, parameter = sim[[x]]$post[[.par]])
  })
  .hline <- if (.par == "R0") {
    2.6
  } else if (.par == "f2") {
    0.4
  } else if (.par == "phi") {
    5
  }
  ggplot(out, aes_string("as.factor(sim)", "parameter")) +
    # geom_boxplot() +
    geom_violin(colour = .hist_blue, fill = NA) +
    geom_violin(fill = .hist_blue, colour = NA, alpha = 0.3) +
    geom_hline(yintercept = .hline) +
    ylab(.par)+xlab("Simulation")
}

g1 <- check_sim_theta("R0")
g2 <- check_sim_theta("phi")
g3 <- check_sim_theta("f2")
cowplot::plot_grid(g1, g2, g3, ncol = 1)
ggsave("figs-ms/R-sim-test-theta.png", width = 6, height = 7)

# # Try fitting to posterior predictive draws: -----------------------------
#
# dat <- readr::read_csv(here::here("nCoVDailyData/CaseCounts/BC Case counts.csv"))
# names(dat)[names(dat) == "BC"] <- "Cases"
# dat$Date[71] <- "1/4/2020" # argh
# dat$Date[72] <- "2/4/2020" # argh
# dat$Date <- lubridate::dmy(dat$Date)
# dat$day <- seq_len(nrow(dat))
# dat$daily_diffs <- c(
#   dat$Cases[2] - dat$Cases[1],
#   diff(dat$Cases)
# )
# dat <- dplyr::filter(dat, Date >= "2020-03-01")
# daily_diffs <- dat$daily_diffs
# plot(daily_diffs)
# fit <- fit_seeiqr(
#   daily_diffs,
#   chains = 1,
#   iter = 300,
#   seed = 129438239,
#   forecast_days = 60,
#   seeiqr_model = seeiqr_model
# )
# print(fit$fit, pars = c("R0", "f2", "phi"))
# fit$post$y_rep[1, ]
#
# plan(multisession)
# ppd_sim <- furrr::future_map(1:16, function(x) {
#   fit_seeiqr(
#     daily_cases = fit$post$y_rep[x, 1:40],
#     chains = 1,
#     seed = 129294,
#     iter = 400, cores = 1,
#     forecast_days = 60,
#     seeiqr_model = seeiqr_model
#   )
# })
# plan(future::sequential)
#
# purrr::walk(ppd_sim, ~ print(.x$fit, pars = c("R0", "f2", "phi")))
#
# check_sim <- function(.par) {
#   out <- purrr::map_df(seq_along(ppd_sim), function(x) {
#     data.frame(sim = x, parameter = ppd_sim[[x]]$post[[.par]])
#   })
#   ggplot(out, aes_string("as.factor(sim)", "parameter")) +
#     geom_boxplot() +
#     geom_hline(yintercept = median(fit$post[[.par]])) +
#     geom_hline(yintercept = quantile(fit$post[[.par]], 0.1)) +
#     geom_hline(yintercept = quantile(fit$post[[.par]], 0.9)) +
#     ylab(.par)
# }
#
# g1 <- check_sim("R0")
# g2 <- check_sim("phi")
# g3 <- check_sim("f2")
# cowplot::plot_grid(g1, g2, g3, nrow = 3)
# ggsave("figs/ppd-sim-test.pdf", width = 6, height = 7)
# ggsave("figs/ppd-sim-test.png", width = 6, height = 7)
#
# # Compare PPD expectations to truth: ----------------------------------------------
#
# out <- purrr::map_df(ppd_sim, function(.x) {
#   temp <- .x$post$lambda_d %>%
#     reshape2::melt() %>%
#     dplyr::rename(day = Var2)
#   temp <- temp %>%
#     group_by(day) %>%
#     summarise(
#       lwr2 = quantile(value, probs = 0.95),
#       upr2 = quantile(value, probs = 0.05),
#       med = median(value)
#     )
#   temp
# }, .id = "simulation")
#
# out_obs <- purrr::map_df(ppd_sim, function(.x) {
#   temp <- .x$post$y_rep %>%
#     reshape2::melt() %>%
#     dplyr::rename(day = Var2)
#   temp <- temp %>%
#     group_by(day) %>%
#     summarise(
#       lwr2 = quantile(value, probs = 0.95),
#       upr2 = quantile(value, probs = 0.05),
#       med = median(value)
#     )
#   temp
# }, .id = "simulation")
#
# truth <- purrr::map_df(seq_along(ppd_sim), function(i) {
#   y_rep <- fit$post$y_rep[i, ]
#   lambda_d <- fit$post$lambda_d[i, ]
#   tibble(day = seq_along(y_rep), y_rep = y_rep, lambda_d = lambda_d)
# }, .id = "simulation")
#
# out %>%
#   mutate(forecast = ifelse(day > 40, TRUE, FALSE)) %>%
#   ggplot(aes(x = day, y = med, ymin = lwr2, ymax = upr2)) +
#   facet_wrap(~simulation, ncol = 4) +
#   geom_point(
#     data = truth,
#     col = "black", inherit.aes = FALSE, aes(x = day, y = y_rep), pch = 21, alpha = 0.5
#   ) +
#   geom_vline(xintercept = 40, lty = 2) +
#   geom_line(
#     data = truth,
#     col = "black", inherit.aes = FALSE, aes(x = day, y = lambda_d), lwd = 0.7) +
#   # geom_ribbon(alpha = 0.4, colour = NA, fill = "blue") +
#   geom_line(alpha = 0.9, lwd = 1, colour = "blue", aes(lty = forecast)) +
#   geom_ribbon(data = out_obs, fill = "blue", alpha = 0.2) +
#   # geom_line(data = out_obs, colour = "blue") +
#   ylab("Cases") + xlab("Day") + labs(lty = "Forecast")
# ggsave("figs/sim-ppd-ts-hat.png", width = 9, height = 5)
#
