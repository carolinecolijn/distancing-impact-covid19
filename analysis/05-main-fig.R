source(here::here("analysis/data-model-prep.R"))

m <- fit_seeiqr(daily_diffs, seeiqr_model = seeiqr_model, iter = 3000, chains = 8)
print(m$fit, pars = c("R0", "f2", "phi"))
saveRDS(m, file = "data-generated/main-fit-2000.rds")
m <- readRDS("data-generated/main-fit-2000.rds")

sd_est <- sprintf("%.0f",
  100 * (1 - round(quantile(m$post$f2, c(0.05, 0.5, 0.95)), 2)))
sd_est_frac <- sprintf("%.2f",
  (1 - round(quantile(m$post$f2, c(0.05, 0.5, 0.95)), 2)))
write_tex(sd_est_frac[1], "fracEstUpr")
write_tex(sd_est_frac[2], "fracEstMed")
write_tex(sd_est_frac[3], "fracEstLwr")

write_tex(sd_est[1], "percEstUpr")
write_tex(sd_est[2], "percEstMed")
write_tex(sd_est[3], "percEstLwr")

sd_est <- sprintf("%.2f", round(quantile(m$post$f2, c(0.05, 0.5, 0.95)), 2))
write_tex(sd_est[3], "fTwoEstUpr")
write_tex(sd_est[2], "fTwoEstMed")
write_tex(sd_est[1], "fTwoEstLwr")

write_tex(sum(daily_diffs) + 8, "totalCases")

make_quick_plots(m, id = "-ms-main", ext = ".png")
file.copy("figs/traceplot-ms-main.png", "figs-ms/traceplots.png",
  overwrite = TRUE)
file.copy("figs/posterior-predictive-case-diffs-facet-ms-main.png",
  "figs-ms/post-pred-reps.png", overwrite = TRUE)

fit_array <- as.array(m$fit)
dimnames(fit_array)[[3]] <- gsub("R0", "R0b", dimnames(fit_array)[[3]])
g <- bayesplot::mcmc_trace(fit_array, pars = c("R0", "f2", "phi[1]"))

# fewer samples for plot:
m500 <- fit_seeiqr(daily_diffs,
  seeiqr_model = seeiqr_model, iter = 500, chains = 8,
  save_state_predictions = TRUE
)
print(m500$fit, pars = c("R0", "f2", "phi"))
saveRDS(m500, file = "data-generated/main-fit-500.rds")
m500 <- readRDS("data-generated/main-fit-500.rds")

proj <- make_projection_plot(list(m))

obj <- m
post <- m$post
R0 <- post$R0

.x <- seq(2.5, 3.5, length.out = 300)
breaks <- seq(min(.x), max(.x), 0.016)
R0_hist <- ggplot(tibble(R0 = R0)) +
  geom_ribbon(
    data = tibble(
      R0 = .x,
      density = dlnorm(.x, obj$R0_prior[1], obj$R0_prior[2])
    ),
    aes(x = R0, ymin = 0, ymax = density), alpha = 0.5, colour = "grey50",
    fill = "grey50"
  ) +
  ylab("Density") +
  geom_histogram(
    breaks = breaks, aes(x = R0, y = ..density..),
    fill = .hist_blue, alpha = .7, colour = "grey90", lwd = 0.15
  ) +
  coord_cartesian(xlim = range(.x), expand = FALSE) +
  xlab(expression(italic(R[0 * plain(b)])))

R0_hist

f2 <- post$f2
.x <- seq(0, 1, length.out = 300)
breaks <- seq(min(.x), max(.x), 0.022)
f2_hist <- ggplot(tibble(f2 = f2)) +
  geom_ribbon(
    data = tibble(
      f2 = .x,
      density = dbeta(.x, obj$f2_prior_beta_shape1, obj$f2_prior_beta_shape2)
    ),
    aes(x = f2, ymin = 0, ymax = density), alpha = 0.5, colour = "grey50",
    fill = "grey50"
  ) +
  geom_histogram(
    breaks = breaks, aes(x = f2, y = ..density..),
    fill = .hist_blue, alpha = .7, colour = "grey90", lwd = 0.15
  ) +
  ylab("Density") +
  coord_cartesian(xlim = range(.x), expand = FALSE) +
  xlab("Fraction of normal contacts") +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  geom_vline(xintercept = .55, lty = 2, col = "grey40")

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
  summarize(
    I = value[variable == "I"], Id = value[variable == "Id"],
    prevalence = I + Id
  ) %>%
  mutate(day = .start + lubridate::ddays(time))

g_prev <- ggplot(prevalence, aes(day, prevalence, group = iterations)) +
  annotate("rect",
    xmin = .start + lubridate::ddays(obj$last_day_obs),
    xmax = .start + lubridate::ddays(obj$last_day_obs + 60),
    ymin = 0, ymax = Inf, fill = "grey95"
  ) +
  geom_line(alpha = 0.05, col = .hist_blue) +
  ylab("Modelled prevalence") +
  coord_cartesian(expand = FALSE, xlim = c(.start, .start + lubridate::ddays(obj$last_day_obs + 60)), ylim = c(0, max(prevalence$prevalence) * 1.04)) +
  xlab("")
g_prev

gg_additions <- list(
  theme(
    plot.margin = margin(t = 0, r = 5, b = 1, l = 5),
    axis.title.y.left = element_text(margin = margin(r = 0)))
)

g <- cowplot::plot_grid(
  proj + gg_additions,
  R0_hist + gg_additions,
  g_prev + gg_additions,
  f2_hist + gg_additions,
  align = "hv",
  labels = "AUTO", label_size = 12, label_x = 0.225, label_y = 0.995) +
  theme(plot.margin = margin(11 / 2, 11 / 2, 11 / 2, 11 / 2))
# ggsave(paste0("figs-ms/fig1.png"), width = 5, height = 4, dpi = 400)
ggsave(paste0("figs-ms/fig1.pdf"), width = 5.25, height = 3.8)

g <- ggplot(states, aes(time, value, group = iterations)) +
  geom_line(alpha = 0.1) +
  facet_wrap(~variable, scales = "free_y") +
  geom_vline(xintercept = obj$last_day_obs, lty = 2, alpha = 0.6) +
  xlab("Time (days starting on March 1, 2020)") +
  ylab("Individuals")
ggsave(paste0("figs-ms/states.png"), width = 12, height = 7, dpi = 200)
