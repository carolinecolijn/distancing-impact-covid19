source(here::here("analysis/data-model-prep.R"))

# -----------------------------------------------------------------------------
# What is the delay between the peak prevalence (I+Id) and the peak in case
# counts?

m_peak <- readRDS("data-generated/main-fit-500.rds")

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
  summarize(
    I = value[variable == "I"], Id = value[variable == "Id"],
    prevalence = I + Id
  ) %>%
  group_by(iterations) %>%
  dplyr::summarise(prevalence_peak = time[prevalence == max(prevalence)])

lambdas <- obj$post$lambda_d %>%
  reshape2::melt() %>%
  dplyr::rename(day = Var2) %>%
  as_tibble() %>%
  rename(case_count = value) %>%
  group_by(iterations) %>%
  dplyr::summarise(case_peak = day[case_count == max(case_count)])

both <- left_join(states, lambdas) %>%
  filter(case_peak != prevalence_peak) # one weird draw?
ggplot(tibble(delay = both$case_peak - both$prevalence_peak), aes(delay)) +
  geom_histogram(bins = 20)
ggsave("figs/delay-peak-prevalence.png", width = 5, height = 3.5)

states_timing <- states %>% mutate(
  start_decline = obj$stan_data$x_r[["start_decline"]],
  end_decline = obj$stan_data$x_r[["end_decline"]]
)
ggplot(states_timing, aes(prevalence_peak - start_decline)) +
  geom_histogram()
ggplot(states_timing, aes(prevalence_peak - end_decline)) +
  geom_histogram()

v <- round(quantile(states_timing$prevalence_peak - states_timing$start_decline, probs = c(0.05, 0.5, 0.95)), 0)

write_tex(v[[1]], "prevDelayFifteenthLwr")
write_tex(v[[2]], "prevDelayFifteenthMed")
write_tex(v[[3]], "prevDelayFifteenthUpr")

v <- round(quantile(states_timing$prevalence_peak - 12, probs = c(0.05, 0.5, 0.95)), 0)

write_tex(v[[1]], "prevDelayTwelfthLwr")
write_tex(v[[2]], "prevDelayTwelfthMed")
write_tex(v[[3]], "prevDelayTwelfthUpr")

# round(mean(both$case_peak - both$prevalence_peak), 1)
v <- round(quantile(both$case_peak - both$prevalence_peak, probs = c(0.05, 0.5, 0.95)), 1)

write_tex(v[[1]], "delayLwr")
write_tex(v[[2]], "delayMed")
write_tex(v[[3]], "delayUpr")

hist(both$prevalence_peak)
p <- sprintf("%.1f", round(quantile(both$prevalence_peak, probs = c(0.05, 0.5, 0.95)), 1))
write_tex(p[[1]], "prevalencePeakLwr")
write_tex(p[[2]], "prevalencePeakMed")
write_tex(p[[3]], "prevalencePeakUpr")
