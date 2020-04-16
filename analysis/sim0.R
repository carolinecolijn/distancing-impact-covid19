library(rstan)
rstan_options(auto_write = TRUE)
library(ggplot2)

x_r <- c(
  N = 5.1e6, # population of BC in thousands
  D = 4,
  k1 = 1 / 4,
  k2 = 1,
  q = 0.05,
  r = 1,
  ur = 0.2,
  f1 = 1.0,
  f2 = 0.4,
  ratio = 0.7/0.2
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

ts <- seq(-30, 30, 0.1)

sim <- stan(
  "sim0.stan",
  data = list(
    T = length(ts),
    y0 = state_0,
    t0 = min(ts) - 0.0000001,
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

# ---------------------------------------------------------------------

source("functions_sir.R")

pars <- list(
  N = 5.1e6, # population of BC
  D = 4,
  R0 = 2.65,
  k1 = 1 / 4,
  k2 = 1,
  q = 0.05,
  r = 1,
  ur = 0.2,
  f1 = 1.0,
  f2 = 0.4,
  ratio = 0.7/0.2 # 2nd stage sampFrac
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
  to = 30,
  by = 0.1
)

example_simulation <- as.data.frame(deSolve::ode(
  y = state_0,
  times = times,
  func = socdistmodel,
  parms = pars,
  sdtiming = sdtiming_gradual
))

states_R <- reshape2::melt(example_simulation, id.vars = "time")
ggplot(states_R, aes(time, value)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y")

head(states_R)
head(states)

library(dplyr)

states_R <- rename(states_R, value_R = value, day = time)
both <- left_join(states_R, select(states, day, variable, value))

filter(both, is.na(value_R))
filter(both, is.na(value))

head(both)

nrow(states_R)
nrow(states)

ggplot(both, aes(day, value)) +
  geom_line() +
  geom_line(aes(y = value_R), col = "red") +
  facet_wrap(~variable, scales = "free_y")

.diff <- both$value_R - both$value

both %>%
  mutate(.diff = (value_R - value)) %>%
  ggplot(aes(day, .diff)) +
  geom_line() +
  facet_wrap(~variable)
