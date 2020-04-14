# library(rstan)
# rstan_options(auto_write = TRUE)
# library(ggplot2)
# library(dplyr)
#
# wd <- getwd()
# setwd(here::here("selfIsolationModel", "stan"))
#
# x_r <- c(
#   N = 4.4e6, # population of BC in thousands
#   D = 5,
#   k1 = 1 / 5,
#   k2 = 1,
#   q = 0.05,
#   r = 1,
#   ur = 0.4,
#   f1 = 1.0,
#   f2 = 0.4,
#   ratio = 2,
#   start_decline = 12,
#   end_decline = 22
# )
#
# theta <- c(R0 = 2.65)
#
# fsi <- x_r[["r"]] / (x_r[["r"]] + x_r[["ur"]])
# nsi <- 1 - fsi
# i0 <- 8
#
# state_0 <- c(
#   S = nsi * (x_r[["N"]] - i0),
#   E1 = 0.4 * nsi * i0,
#   E2 = 0.1 * nsi * i0,
#   I = 0.5 * nsi * i0,
#   Q = 0,
#   R = 0,
#   Sd = fsi * (x_r[["N"]] - i0),
#   E1d = 0.4 * fsi * i0,
#   E2d = 0.1 * fsi * i0,
#   Id = 0.5 * fsi * i0,
#   Qd = 0,
#   Rd = 0
# )
#
# time <- seq(-30, 30, 0.1)
# days <- seq(0, 30)
#
# stopifnot(
#   names(x_r) ==
#     c("N", "D", "k1", "k2", "q", "r", "ur", "f1", "f2", "ratio", "start_decline", "end_decline")
# )
# stopifnot(
#   names(state_0) == c("S", "E1", "E2", "I", "Q", "R", "Sd", "E1d", "E2d", "Id", "Qd", "Rd")
# )
#
# get_time_id <- function(day, time) max(which(time < day))
# time_day_id <- vapply(days, get_time_id, numeric(1), time = time)
#
# # x_r[['start_decline']] <- max(which(time < x_r[['start_decline']]))
# # x_r[['end_decline']] <- max(which(time < x_r[['end_decline']]))
#
# sim <- stan(
#   "seeiqr-sim.stan",
#   data = list(
#     T = length(time),
#     days = days,
#     N = length(days),
#     y0 = state_0,
#     t0 = min(time) - 1,
#     time = time,
#     theta = array(theta),
#     x_r = x_r,
#     delayShape = 1.972,
#     delayScale = 12.053,
#     sampFrac = rep(0.3, length(time)),
#     time_day_id = time_day_id
#   ),
#   algorithm = "Fixed_param",
#   iter = 1L,
#   chains = 1L
# )
#
# post <- rstan::extract(sim)
#
# variables_df <- dplyr::tibble(variable = names(state_0), variable_num = seq_along(state_0))
# ts_df <- dplyr::tibble(time = time, time_num = seq_along(time))
# states <- reshape2::melt(post$y_hat) %>%
#   dplyr::rename(time_num = Var2, variable_num = Var3) %>%
#   dplyr::left_join(variables_df, by = "variable_num") %>%
#   dplyr::left_join(ts_df, by = "time_num")
#
# ggplot(states, aes(time, value)) +
#   geom_line() +
#   facet_wrap(~variable, scales = "free_y")
#
# dplyr::tibble(day = days, lambda_d = post$lambda_d[1,]) %>%
#   ggplot(aes(days, lambda_d)) +
#   geom_line()
#
# setwd(wd)
