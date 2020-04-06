# start by sourcing `02-seeiqr-fit.R`

library(ggplot2)
library(dplyr)
theme_set(theme_light())

wd <- getwd()
setwd(here::here("selfIsolationModel", "stan"))
dir.create("figs", showWarnings = FALSE)

fit_array <- as.array(fit)
bayesplot::mcmc_trace(fit_array, pars = c("theta[1]", "phi"))
ggsave("figs/traceplot.pdf", width = 6, height = 3)

R0 <- post$theta[, 1]
.x <- seq(1.3, 3.8, length.out = 200)
breaks <- seq(min(.x), max(.x), 0.05)
ggplot(tibble(R0 = R0)) +
  geom_ribbon(
    data = tibble(R0 = .x, density = dlnorm(.x, R0_prior[1], R0_prior[2])),
    aes(x = R0, ymin = 0, ymax = density), alpha = 0.5, colour = "grey50",
    fill = "grey50"
  ) +
  geom_histogram(
    breaks = breaks, aes(x = R0, y = ..density..),
    fill = "blue", alpha = 0.5
  ) +
  coord_cartesian(xlim = range(.x), expand = FALSE)
ggsave("figs/R0.pdf", width = 6, height = 4)

phi_hat <- post$phi
.x <- seq(0.1, 4, length.out = 200)
breaks <- seq(min(.x), max(.x), 0.08)
ggplot(tibble(phi = phi_hat)) +
  geom_ribbon(
    data = tibble(phi = .x, density = dlnorm(.x, log(1), 0.5)),
    aes(x = phi, ymin = 0, ymax = density),
    alpha = 0.5, colour = "grey50", fill = "grey50"
  ) +
  geom_histogram(
    breaks = breaks, aes(x = phi, y = ..density..),
    fill = "blue", alpha = 0.5
  ) +
  coord_cartesian(xlim = range(.x), expand = FALSE)
ggsave("figs/phi.pdf", width = 6, height = 4)

draws <- sample(seq_along(post$lambda_d[, 1]), 100L)
variables_df <- dplyr::tibble(
  variable = names(state_0),
  variable_num = seq_along(state_0)
)
ts_df <- dplyr::tibble(time = time, time_num = seq_along(time))
states <- reshape2::melt(post$y_hat) %>%
  dplyr::rename(time_num = Var2, variable_num = Var3) %>%
  dplyr::filter(iterations %in% draws) %>%
  dplyr::left_join(variables_df, by = "variable_num") %>%
  dplyr::left_join(ts_df, by = "time_num")

ggplot(states, aes(time, value, group = iterations)) +
  geom_line(alpha = 0.1) +
  facet_wrap(~variable, scales = "free_y") +
  geom_vline(xintercept = last_day_obs, lty = 2, alpha = 0.6)
ggsave("figs/states.pdf", width = 12, height = 7.5)

draws <- sample(seq_along(post$lambda_d[, 1]), 400L)
reshape2::melt(post$lambda_d) %>%
  dplyr::rename(day = Var2) %>%
  dplyr::filter(iterations %in% draws) %>%
  ggplot(aes(day, value, group = iterations)) +
  geom_line(alpha = 0.05) +
  geom_point(
    data = tibble(day = seq_along(daily_diffs), value = daily_diffs),
    inherit.aes = FALSE, aes(x = day, y = value)
  )
ggsave("figs/expected-case-diffs.pdf", width = 6, height = 4)

# Posterior predictive checks:

draws <- sample(seq_along(post$y_rep[, 1]), 100L)
post$y_rep %>%
  reshape2::melt() %>%
  dplyr::filter(iterations %in% draws) %>%
  dplyr::rename(day = Var2) %>%
  ggplot(aes(day, value, group = iterations)) +
  geom_line(alpha = 0.1) +
  geom_line(
    data = tibble(day = seq_len(last_day_obs), value = daily_diffs),
    col = "red", inherit.aes = FALSE, aes(x = day, y = value)
  )
ggsave("figs/posterior-predictive-case-diffs.pdf", width = 6, height = 4)

set.seed(1929)
draws <- sample(seq_along(post$y_rep[, 1]), 24L)
post$y_rep %>%
  reshape2::melt() %>%
  dplyr::filter(iterations %in% draws) %>%
  dplyr::rename(day = Var2) %>%
  mutate(Type = "Posterior\npredictive\nsimulation") %>%
  bind_rows(tibble(
    iterations = 0, day = seq_len(last_day_obs),
    value = daily_diffs, Type = "Observed"
  )) %>%
  ggplot(aes(day, value, colour = Type)) +
  geom_line(lwd = 0.7) +
  facet_wrap(vars(iterations)) +
  ylab("New cases") +
  xlab("Day") +
  scale_color_manual(values = c("red", "grey40")) +
  geom_vline(xintercept = last_day_obs, lty = 2, alpha = 0.6)
ggsave("figs/posterior-predictive-case-diffs-facet.pdf", width = 9, height = 6.25)

post$y_rep %>%
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
  ggplot(aes(day, y = med, ymin = lwr, ymax = upr)) +
  geom_ribbon(alpha = 0.2) +
  geom_ribbon(alpha = 0.2, mapping = aes(ymin = lwr2, ymax = upr2)) +
  geom_line(alpha = 0.9, lwd = 1) +
  geom_line(
    data = tibble(day = seq_len(last_day_obs), value = daily_diffs),
    col = "red", inherit.aes = FALSE, aes(x = day, y = value), lwd = 0.5
  ) +
  ylab("New cases") +
  xlab("Day") +
  geom_vline(xintercept = last_day_obs, lty = 2, alpha = 0.6)
ggsave("figs/posterior-predictive-quantiles-case-diffs.pdf", width = 6, height = 4)

setwd(wd)
