source("data-model-prep.R")

m <- fit_seeiqr(daily_diffs, seeiqr_model = seeiqr_model, iter = 2000, chains = 8)
print(m$fit, pars = c("R0", "f2", "phi"))
saveRDS(m, file = "data-generated/main-fit-2000.rds")
m <- readRDS("data-generated/main-fit-2000.rds")

source("make_quick_plots.R")
make_quick_plots(m, id = "-ms-main", ext = ".png")
file.copy("figs/traceplot-ms-main.png", "figs-ms/traceplots.png")
file.copy("figs/posterior-predictive-case-diffs-facet-ms-main.png", "figs-ms/post-pred-reps.png")

# fewer samples for plot:
m500 <- fit_seeiqr(daily_diffs, seeiqr_model = seeiqr_model, iter = 500, chains = 8,
  save_state_predictions = TRUE)
print(m500$fit, pars = c("R0", "f2", "phi"))
saveRDS(m500, file = "data-generated/main-fit-500.rds")
# m500 <- readRDS("data-generated/main-fit-500.rds")

proj <- make_projection_plot(list(m))

obj <- m
post <- m$post
R0 <- post$R0

.hist_blue <- RColorBrewer::brewer.pal(6, "Blues")[5]

.x <- seq(2.1, 2.9, length.out = 300)
breaks <- seq(min(.x), max(.x), 0.016)
R0_hist <- ggplot(tibble(R0 = R0)) +
  geom_ribbon(
    data = tibble(R0 = .x,
      density = dlnorm(.x, obj$R0_prior[1], obj$R0_prior[2])),
    aes(x = R0, ymin = 0, ymax = density), alpha = 0.5, colour = "grey50",
    fill = "grey50"
  ) +
  ylab("Density") +
  geom_histogram(
    breaks = breaks, aes(x = R0, y = ..density..),
    fill = .hist_blue, alpha = .7, colour = "grey90", lwd = 0.15
  ) +
  coord_cartesian(xlim = range(.x), expand = FALSE)

R0_hist

f2 <- post$f2
.x <- seq(0, 1, length.out = 300)
breaks <- seq(min(.x), max(.x), 0.022)
f2_hist <- ggplot(tibble(f2 = f2)) +
  geom_ribbon(
    data = tibble(f2 = 1 - .x,
      density = dbeta(.x, obj$f2_prior_beta_shape1, obj$f2_prior_beta_shape2)),
    aes(x = f2, ymin = 0, ymax = density), alpha = 0.5, colour = "grey50",
    fill = "grey50"
  ) +
  geom_histogram(
    breaks = breaks, aes(x = 1-f2, y = ..density..),
    fill = .hist_blue, alpha = .7, colour = "grey90", lwd = 0.15
  ) +
  ylab("Density") +
  coord_cartesian(xlim = range(.x), expand = FALSE) +
  xlab("Physical distancing strength") +
  scale_x_continuous(breaks = seq(0, 1, 0.2))

f2_hist

obj <- m500
post <- obj$post
draws <- 1:250
ts_df <- dplyr::tibble(time = obj$time, time_num = seq_along(obj$time))
variables_df <- dplyr::tibble(
  variable = names(obj$state_0),
  variable_num = seq_along(obj$state_0)
)
ts_df <- dplyr::tibble(time = obj$time, time_num = seq_along(obj$time))
states <- reshape2::melt(post$y_hat) %>%
  dplyr::rename(time_num = Var2, variable_num = Var3) %>%
  dplyr::filter(iterations %in% draws) %>%
  dplyr::left_join(variables_df, by = "variable_num") %>%
  dplyr::left_join(ts_df, by = "time_num")

.start <- lubridate::ymd_hms("2020-03-01 00:00:00")
prevalence <- states %>%
  dplyr::filter(variable %in% c("I", "Id")) %>%
  group_by(iterations, time) %>%
  summarize(I = value[variable == "I"], Id = value[variable == "Id"],
    prevalence = I + Id) %>%
  mutate(day = .start + lubridate::ddays(time))

g_prev <- ggplot(prevalence, aes(day, prevalence, group = iterations)) +
  annotate("rect", xmin = .start + lubridate::ddays(obj$last_day_obs),
    xmax = .start + lubridate::ddays(obj$last_day_obs + 60), ymin = 0, ymax = Inf, fill = "grey95") +
  geom_line(alpha = 0.05, col = .hist_blue) +
  ylab("Prevalence") +
  coord_cartesian(expand = FALSE, xlim = c(.start, .start + lubridate::ddays(obj$last_day_obs + 60)), ylim = c(0, max(prevalence$prevalence) * 1.04)) +
  xlab("")
g_prev

g <- cowplot::plot_grid(proj, R0_hist, g_prev, f2_hist, align = "hv", labels = "AUTO") +   theme(plot.margin = margin(11/2,11, 11/2, 11/2))
ggsave(paste0("figs-ms/fig1.png"), width = 6, height = 4.5)

g <- ggplot(states, aes(time, value, group = iterations)) +
  geom_line(alpha = 0.1) +
  facet_wrap(~variable, scales = "free_y") +
  geom_vline(xintercept = obj$last_day_obs, lty = 2, alpha = 0.6) +
  xlab("Time (days since March 1 2010)") + ylab("Individuals")
ggsave(paste0("figs-ms/states.png"), width = 12, height = 7)

