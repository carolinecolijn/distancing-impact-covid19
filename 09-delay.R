
# -----------------------------------------------------------------------------
# What is the delay between the peak prevalence (I+Id) and the peak in case counts?

x <- seq(0, 25, length.out = 200);plot(x, dweibull(x, shape = 2, scale = 11), type = "l")
x <- seq(0, 25, length.out = 200);plot(x, dweibull(x, shape = 2, scale = 11), type = "l")
m_peak <- fit_seeiqr(
  daily_diffs, sampled_fraction1 = 0.1, sampled_fraction2 = 0.3,
  seeiqr_model = seeiqr_model, chains = 8, iter = 600,
  delayScale = 11,
  save_state_predictions = TRUE)
obj <- m_peak
post <- obj$post
variables_df <- dplyr::tibble(
  variable = names(obj$state_0),
  variable_num = seq_along(obj$state_0)
)
ts_df <- dplyr::tibble(time = obj$time, time_num = seq_along(obj$time))
states <- reshape2::melt(post$y_hat) %>%
  dplyr::rename(time_num = Var2, variable_num = Var3) %>%
  dplyr::left_join(variables_df, by = "variable_num") %>%
  dplyr::left_join(ts_df, by = "time_num") %>%
  as_tibble() %>%
  mutate(day = floor(time)) %>%
  dplyr::filter(variable %in% c("I", "Id")) %>%
  group_by(iterations, time) %>%
  summarize(I = value[variable == "I"], Id = value[variable == "Id"],
    prevalence = I + Id) %>%
  group_by(iterations) %>%
  dplyr::summarise(prevalence_peak = time[prevalence == max(prevalence)])

lambdas <- obj$post$lambda_d %>%
  reshape2::melt() %>%
  dplyr::rename(day = Var2) %>% as_tibble() %>%
  rename(case_count = value) %>%
  group_by(iterations) %>%
  dplyr::summarise(case_peak = day[case_count == max(case_count)])

both <- left_join(states, lambdas) %>%
  filter(case_peak != prevalence_peak) # one weird draw?
ggplot(tibble(delay = both$case_peak - both$prevalence_peak), aes(delay)) +
  geom_histogram(bins = 20)
ggsave("figs/delay-peak-prevalence.png", width = 5, height = 3.5)

states_timing <- states %>% mutate(start_decline = obj$stan_data$x_r[['start_decline']],
  end_decline = obj$stan_data$x_r[['end_decline']])
ggplot(states_timing, aes(prevalence_peak - start_decline)) + geom_histogram()
ggplot(states_timing, aes(prevalence_peak - end_decline)) + geom_histogram()
