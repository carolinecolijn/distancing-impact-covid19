source(here::here("analysis/data-model-prep.R"))

# Identify the point at which prevalence slope goes to 0: ---------------------

fs <- seq(0.2, 1, 0.2)
plan(multisession, workers = parallel::detectCores()/2)
m_fs <- furrr::future_map(fs, function(.f) {
  fit_seeiqr(
    daily_diffs, iter = 400, chains = 1, save_state_predictions = TRUE,
      seeiqr_model = seeiqr_model, fixed_f_forecast = .f)
})
plan(future::sequential)

slopes <- purrr::map2_df(m_fs, fs, get_prevalence_slope)
ggplot(slopes, aes(f, slope)) +
  geom_point(alpha = 0.1)

mlm <- lm(slope ~ f, data = slopes)
nd <- data.frame(f = seq(0, 1, length.out = 5000))
nd$predicted_slope <- predict(mlm, newdata = nd)
thresh <- dplyr::filter(nd, predicted_slope > 0) %>% `[`(1, 'f')
thresh
ggplot(slopes, aes(f, slope)) +
  geom_point(alpha = 0.04) +
  geom_line(data = nd, aes(f, predicted_slope), alpha = 0.3) +
  geom_vline(xintercept = thresh, lty = 2, alpha = 0.6) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.6) +
  ylab("Slope of log(prevalence) vs. day") +
  xlab("Fraction of normal contacts")
ggsave("figs-ms/f-threshold.png", width = 3.7, height = 3.5)

# write_tex(round(1 - thresh, 2) * 100, "thresholdPerc")
# write_tex(round(1 - thresh, 2), "thresholdFrac")
write_tex(round(thresh, 2), "thresholdFtwo")
saveRDS(thresh, file = here::here("data-generated/BC-threshold.rds"))

# Joint posterior plot with prevalence colouring: -----------------------------

m_yhat <- fit_seeiqr(
  daily_diffs, iter = 200, chains = 6, save_state_predictions = TRUE,
  seeiqr_model = seeiqr_model)

joint_post <- tibble(R0 = m_yhat$post$R0, f2 = m_yhat$post$f2, iterations = seq_along(f2))
prev_slopes <- get_prevalence_slope(m_yhat, "estimated") %>%
  mutate(perc_change = 100 * (exp(slope) - 1))
joint_post2 <- left_join(joint_post, prev_slopes)
g <- ggplot(joint_post2, aes(R0, f2, colour = -perc_change)) +
  geom_point(alpha = 0.1, size = 2) +
  geom_point(alpha = 0.2, size = 2, pch = 21) +
  scale_colour_viridis_c(option = "D", direction = -1) +
  labs(colour = "Percent decline\nper day", y = "Fraction of normal contacts",
    x = expression(italic(R[0 * plain(b)]))) +
  # theme(legend.position = c(0.81, 0.78)) +
  theme(legend.key.size = unit(11, units = "points"))
ggsave("figs-ms/joint-posterior-prevalence.png", width = 4.7, height = 3.5)
ggsave("figs-ms/joint-posterior-prevalence.pdf", width = 4.7, height = 3.5)
