source("data-model-prep.R")

# sampFrac random walk --------------------------------------------------------

m_rw <- fit_seeiqr(
  daily_diffs,
  sampFrac2_type = "rw",
  rw_sigma = 0.1,
  sampFrac2_prior = c(0.3, 0.3),
  sampled_fraction_day_change = 5,
  forecast_days = 60,
  seeiqr_model = seeiqr_model, chains = 8, iter = 1000)
m_rw$post$sampFrac2 %>% reshape2::melt() %>% as_tibble() %>%
  rename(day = Var2) %>%
  group_by(day) %>%
  summarise(
    lwr = quantile(value, probs = 0.025),
    lwr2 = quantile(value, probs = 0.25),
    upr = quantile(value, probs = 0.975),
    upr2 = quantile(value, probs = 0.75),
    med = median(value)
  ) %>%
  ggplot(aes(day, med)) + geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_ribbon(aes(ymin = lwr2, ymax = upr2), alpha = 0.7) +
  ylab("Sampled fraction") + xlab("Days after March 4")
ggsave(paste0("figs-ms/sampFrac2-rw.png"), width = 5, height = 4)
