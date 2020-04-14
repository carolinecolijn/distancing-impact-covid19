source("data-model-prep.R")
library(future)
# Identify the point at which prevalence slope goes to 0: ---------------------

fs <- seq(0.3, 1, 0.1)
plan(multisession, workers = parallel::detectCores()/2)
m_fs <- furrr::future_map(fs, function(.f) {
  fit_seeiqr(
    daily_diffs, iter = 400, chains = 1, save_state_predictions = TRUE,
      seeiqr_model = seeiqr_model, fixed_f_forecast = .f)
})
plan(future::sequential)
# saveRDS(m_fs, file = "data-generated/fits-for-prevalence-slope.rds")
# m_fs <- readRDS("data-generated/fits-for-prevalence-slope.rds")

get_prevalence_slope <- function(obj, f_val) {
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
    as_tibble()
  temp <- states %>%
    dplyr::filter(time > max(states$time) - 30, variable %in% c("I", "Id")) %>%
    group_by(iterations, time) %>%
    summarize(I = value[variable == "I"], Id = value[variable == "Id"],
      prevalence = I + Id)
  iters <- temp %>% group_by(iterations) %>% summarise(iter = iterations[[1]])
  temp %>%
    group_by(iterations) %>%
    group_split() %>%
    purrr::map(~lm(log(prevalence) ~ time, data = .x)) %>%
    purrr::map_df(~tibble(slope = coef(.x)[[2]])) %>%
    mutate(f = f_val) %>%
    ungroup() %>%
    mutate(iterations = iters$iter)
}
slopes <- purrr::map2_df(m_fs, fs, get_prevalence_slope)
# ggplot(slopes, aes(f, slope)) +
#   geom_jitter(height = 0, alpha = 0.1, width = 0.2)

slopes$inverse_f <- 1-slopes$f
mlm <- lm(slope ~ inverse_f, data = slopes)
nd <- data.frame(inverse_f = 1- seq(0.3, 1, length.out = 5000))
nd$predicted_slope <- predict(mlm, newdata = nd)
thresh <- dplyr::filter(nd, predicted_slope > 0) %>% `[`(1, 'inverse_f')
thresh
ggplot(slopes, aes(inverse_f, slope)) +
  geom_point(alpha = 0.04) +
  geom_line(data = nd, aes(inverse_f, predicted_slope), alpha = 0.3) +
  geom_vline(xintercept = thresh, lty = 2, alpha = 0.6) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.6) +
  ylab("Slope of log(prevalence) vs. day") +
  xlab("Fraction contacts removed")
ggsave("figs-ms/f-threshold.png", width = 3.7, height = 3.5)

write_tex(round(1 - thresh, 2) * 100, "thresholdFrac") %>%
  readr::write_lines("figs-ms/values.tex", append = TRUE)
write_tex(round(thresh, 2), "thresholdFtwo") %>%
  readr::write_lines("figs-ms/values.tex", append = TRUE)

# Joint posterior plot with prevalence colouring: -----------------------------

# m_yhat <- fit_seeiqr(
#   daily_diffs, iter = 200, chains = 8, save_state_predictions = TRUE,
#   seeiqr_model = seeiqr_model)
m_yhat <- readRDS("data-generated/main-fit-500.rds")

joint_post <- tibble(R0 = m_yhat$post$R0, f2 = m_yhat$post$f2, iterations = seq_along(f2))
prev_slopes <- get_prevalence_slope(m_yhat, "estimated") %>%
  mutate(perc_change = 100 * (exp(slope) - 1))
joint_post2 <- left_join(joint_post, prev_slopes)
g <- ggplot(joint_post2, aes(R0, 1-f2, colour = -perc_change)) +
  geom_point(alpha = 0.1, size = 2) +
  geom_point(alpha = 0.2, size = 2, pch = 21) +
  scale_colour_viridis_c(option = "D", direction = -1) +
  labs(colour = "Percent decline\nper day", y = "Fraction contacts removed",
    x = expression(R[0])) +
  # theme(legend.position = c(0.81, 0.78)) +
  theme(legend.key.size = unit(11, units = "points"))
ggsave("figs-ms/joint-posterior-prevalence.png", width = 4.7, height = 3.5)
