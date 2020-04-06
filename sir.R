library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(ggplot2)
library(dplyr)
theme_set(theme_light())

wd <- getwd()
setwd(here::here("selfIsolationModel", "stan"))

x_r <- c(
  N = 4.4e6 / 1e3, # population of BC in thousands
  D = 5,
  k1 = 1 / 5,
  k2 = 1,
  q = 0.05,
  r = 1,
  ur = 0.4,
  f1 = 1.0,
  f2 = 0.4,
  start_decline = 12,
  end_decline = 22
)

fsi <- x_r[["r"]] / (x_r[["r"]] + x_r[["ur"]])
nsi <- 1 - fsi
i0 <- 8

state_0 <- c(
  S = nsi * (x_r[["N"]] - i0),
  E1 = 0.4 * nsi * i0,
  E2 = 0.1 * nsi * i0,
  I = 0.5 * nsi * i0,
  Q = 0,
  R = 0,
  Sd = fsi * (x_r[["N"]] - i0),
  E1d = 0.4 * fsi * i0,
  E2d = 0.1 * fsi * i0,
  Id = 0.5 * fsi * i0,
  Qd = 0,
  Rd = 0
)

stopifnot(
  names(x_r) ==
    c("N", "D", "k1", "k2", "q", "r", "ur", "f1", "f2", "start_decline", "end_decline")
)
stopifnot(
  names(state_0) == c("S", "E1", "E2", "I", "Q", "R", "Sd", "E1d", "E2d", "Id", "Qd", "Rd")
)

daily_diffs <- c(
  0L, 0L, 1L, 3L, 1L, 8L, 0L, 6L, 5L, 0L, 7L, 7L, 18L, 9L, 22L,
  38L, 53L, 45L, 40L, 77L, 76L, 48L, 67L, 78L, 42L, 66L, 67L, 92L,
  16L, 70L, 43L, 53L
)
days <- seq_along(daily_diffs)
time <- seq(-30, max(days), 0.25)
get_time_id <- function(day, time) max(which(time < day))
time_day_id <- vapply(days, get_time_id, numeric(1), time = time)

get_time_id_dx_start <- function(day, time, days_back) {
  check <- time < (day - days_back)
  if (sum(check) == 0L) {
    1L
  } else {
    max(which(check))
  }
}
time_day_id_dx_start <- vapply(days, get_time_id_dx_start, numeric(1),
  time = time, days_back = 50L)

sampFrac <- ifelse(seq_along(time) < time_day_id[14], 0.35, 0.35 * 2)

# Informative R0 prior example:
# https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-3-transmissibility-of-covid-19/
# 2.6 (uncertainty range: 1.5-3.5)
# x <- rlnorm(1e5, meanlog = log(2.55), sdlog = 0.2)
# mean(x)
# quantile(x, probs = c(0.025, 0.5, 0.975))
R0_prior <- c(log(2.6), 0.2)

stan_data <- list(
  T = length(time),
  days = days,
  daily_diffs = daily_diffs,
  offset = rep(log(1), length(days)),
  N = length(days),
  y0 = state_0,
  t0 = min(time) - 1,
  time = time,
  x_r = x_r,
  delayShape = 1.972,
  delayScale = 12.053,
  sampFrac = rep(0.3, length(time)),
  time_day_id = time_day_id,
  time_day_id_dx_start = time_day_id_dx_start,
  R0_prior = R0_prior,
  phi_prior = c(log(1), 0.5),
  priors_only = 0L
)

sir_model <- stan_model("sir.stan")
map_estimate <- optimizing(
  sir_model,
  data = stan_data
)
map_estimate$par['theta[1]']
map_estimate$par['phi']
# map_estimate$par['lambda_d[1]']

initf <- function() {
  list(
    theta = array(rlnorm(1, log(map_estimate$par[['theta[1]']]), 0.1)),
    phi = rlnorm(1, log(map_estimate$par[['phi']]), 0.1)
  )
}
fit <- sampling(
  sir_model,
  data = stan_data,
  iter = 800L,
  chains = 4L,
  init = initf,
  seed = 29482,
  pars = c("theta", "phi", "lambda_d", "y_hat")
)

print(fit, pars = c("theta", "phi"))
post <- rstan::extract(fit)

R0 <- post$theta[, 1]
.x <- seq(1.3, 3.8, length.out = 200)
breaks <- seq(min(.x), max(.x), 0.05)
ggplot(tibble(R0 = R0)) +
  geom_ribbon(
    data = tibble(R0 = .x, density = dlnorm(.x, R0_prior[1], R0_prior[2])),
    aes(x = R0, ymin = 0, ymax = density), alpha = 0.5, colour = "grey50",
    fill = "grey50"
  ) +
  geom_histogram(breaks = breaks, aes(x = R0, y = ..density..), fill = "blue", alpha = 0.5) +
  coord_cartesian(xlim = range(.x), expand = FALSE)
ggsave("R0.pdf", width = 6, height = 4)

phi_hat <- post$phi
.x <- seq(0.1, 4, length.out = 200)
breaks <- seq(min(.x), max(.x), 0.08)
ggplot(tibble(phi = phi_hat)) +
  geom_ribbon(
    data = tibble(phi = .x, density = dlnorm(.x, log(1), 0.5)),
    aes(x = phi, ymin = 0, ymax = density), alpha = 0.5, colour = "grey50", fill = "grey50"
  ) +
  geom_histogram(breaks = breaks, aes(x = phi, y = ..density..), fill = "blue", alpha = 0.5) +
  coord_cartesian(xlim = range(.x), expand = FALSE)
ggsave("phi.pdf", width = 6, height = 4)

draws <- sample(seq_along(post$lambda_d[, 1]), 100L)
variables_df <- dplyr::tibble(variable = names(state_0), variable_num = seq_along(state_0))
ts_df <- dplyr::tibble(time = time, time_num = seq_along(time))
states <- reshape2::melt(post$y_hat) %>%
  dplyr::rename(time_num = Var2, variable_num = Var3) %>%
  dplyr::filter(iterations %in% draws) %>%
  dplyr::left_join(variables_df, by = "variable_num") %>%
  dplyr::left_join(ts_df, by = "time_num")

ggplot(states, aes(time, value, group = iterations)) +
  geom_line(alpha = 0.1) +
  facet_wrap(~variable, scales = "free_y")
ggsave("states.pdf", width = 12, height = 7.5)

draws <- sample(seq_along(post$lambda_d[, 1]), 500L)
reshape2::melt(post$lambda_d) %>%
  dplyr::rename(day = Var2) %>%
  dplyr::filter(iterations %in% draws) %>%
  ggplot(aes(day, value, group = iterations)) +
  geom_line(alpha = 0.05) +
  geom_point(
    data = tibble(day = seq_along(daily_diffs), value = daily_diffs),
    inherit.aes = FALSE, aes(x = day, y = value)
  )
ggsave("expected-case-diffs.pdf", width = 6, height = 4)

setwd(wd)
