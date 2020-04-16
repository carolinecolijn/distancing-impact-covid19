source(here::here("analysis/data-model-prep.R"))

# sampFrac random walk --------------------------------------------------------

m_rw <- fit_seeiqr(
  daily_diffs,
  sampFrac2_type = "rw",
  rw_sigma = 0.1,
  sampFrac2_prior = c(0.2, 0.2),
  sampled_fraction_day_change = 5,
  forecast_days = 60,
  seeiqr_model = seeiqr_model, chains = 4, iter = 1000
)
saveRDS(m_rw, file = "data-generated/rw-fit.rds")
m_rw <- readRDS("data-generated/rw-fit.rds")

.days <- seq(lubridate::ymd("2020-03-01"),
  lubridate::ymd("2020-03-01") + 42, by = "1 day")
g1 <- m_rw$post$sampFrac2 %>%
  reshape2::melt() %>%
  as_tibble() %>%
  rename(day = Var2) %>%
  group_by(day) %>%
  summarise(
    lwr = quantile(value, probs = 0.05),
    lwr2 = quantile(value, probs = 0.25),
    upr = quantile(value, probs = 0.95),
    upr2 = quantile(value, probs = 0.75),
    med = median(value)
  ) %>%
  ggplot(aes(.days[day + 4], med)) +
  geom_line(lwd = 0.8) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_ribbon(aes(ymin = lwr2, ymax = upr2), alpha = 0.5) +
  xlim(.days[1], max(.days) + 60) +
  coord_cartesian(expand = FALSE, ylim = c(0, 1)) +
  ylab("Estimated sampled fraction") +
  xlab("")

g2 <- make_projection_plot(list(m_rw))

cowplot::plot_grid(g1, g2, ncol = 1, align = "hv", labels = "AUTO")
ggsave(paste0("figs-ms/sampFrac2-rw.png"), width = 5, height = 6)
