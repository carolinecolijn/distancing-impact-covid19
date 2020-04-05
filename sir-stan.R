library(rstan)
rstan_options(auto_write = TRUE)
library(ggplot2)

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
  ratio = 2
)

theta <- c(R0 = 2.65)

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

ts <- seq(-30, 30)

sim <- stan(
  "sir.stan",
  data = list(
    T = length(ts),
    y0 = state_0,
    t0 = min(ts) - 1,
    ts = ts,
    theta = array(theta),
    x_r = x_r
  ),
  algorithm = "Fixed_param",
  iter = 1L,
  chains = 1L
)

post <- extract(sim)

variables_df <- dplyr::tibble(variable = names(state_0), variable_num = seq_along(state_0))
ts_df <- dplyr::tibble(day = ts, day_num = seq_along(ts))
states <- reshape2::melt(post$y_hat) %>%
  dplyr::rename(day_num = Var2, variable_num = Var3) %>%
  dplyr::left_join(variables_df, by = "variable_num") %>%
  dplyr::left_join(ts_df, by = "day_num")

ggplot(states, aes(day, value)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y")
