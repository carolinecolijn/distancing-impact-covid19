source(here::here("analysis/data-model-prep.R"))

pars <- list(
  N = 5.1e6, # population of BC
  D = 5,
  R0 = 2.6,
  k1 = 1 / 5,
  k2 = 1,
  q = 0.05,
  r = 0.1,
  ur = 0.02,
  f1 = 1.0,
  f2 = 0.4,
  ratio = 0.3 / 0.1 # 2nd stage sampFrac
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
  sim_dat <- data.frame(
    day = seq(1, max(times)),
    lambda_d = lambda_d,
    obs = MASS::rnegbin(max(times), lambda_d, theta = 5)
  )
  sim_dat
})

plan(multisession, workers = parallel::detectCores() / 2)
sim <- furrr::future_map(seq_along(sim_dat), function(x) {
  fit_seeiqr(
    daily_cases = sim_dat[[x]]$obs,
    seeiqr_model = seeiqr_model,
    forecast_days = 1,
    sampFrac2_type = "fixed",
    sampled_fraction1 = 0.1,
    sampled_fraction2 = 0.3,
    iter = 800,
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

sim_dat_all <- bind_rows(sim_dat, .id = "simulation") %>%
  as_tibble() %>%
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
    alpha = 1, lty = 2
  ) +
  ylab("Simulated reported cases") +
  xlab("Day")
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
  facet_wrap(~simulation, ncol = 4) +
  geom_line(
    data = sim_dat[[1]],
    col = "black", inherit.aes = FALSE, aes(x = day, y = lambda_d), lwd = 0.3,
    alpha = 0.8
  )
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
    geom_violin(colour = .hist_blue, fill = NA) +
    geom_violin(fill = .hist_blue, colour = NA, alpha = 0.3) +
    geom_hline(yintercept = .hline) +
    ylab(.par) +
    xlab("Simulation")
}

g1 <- check_sim_theta("R0") + ylab(expression(italic(R[0 * plain(b)])))
g2 <- check_sim_theta("phi") + coord_cartesian(ylim = c(0, 30)) + ylab(expression(phi))
g3 <- check_sim_theta("f2") + ylab(expression(f[2]))
cowplot::plot_grid(g1, g2, g3, ncol = 1)
ggsave("figs-ms/R-sim-test-theta.png", width = 6, height = 7)
