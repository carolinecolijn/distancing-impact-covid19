source("data-model-prep.R")

m <- fit_seeiqr(
  daily_diffs,
  seeiqr_model = seeiqr_model,
  iter = 400, chains = 7)

make_projection_plot(list(m))

# sd_strength <- seq(0, 1, 0.2) %>% purrr::set_names()
# m_bccdc <- purrr::map(sd_strength, ~ {
#   fit_seeiqr(
#     daily_diffs,
#     sampled_fraction1 = 0.1,
#     sampled_fraction2 = 0.3,
#     f2_prior = c(0.4, 0.2),
#     R0_prior = c(log(2.6), 0.2),
#     sampFrac2_type = "fixed",
#     fixed_f_forecast = .x,
#     delayScale = 11,
#     seeiqr_model = seeiqr_model, chains = 6, iter = 600
#   )
# })


# -----------------------------------------------------------------------------
#
# length(daily_diffs)
# sampled_fraction_vec <- c(
#   rep(0.2, 14),
#   approx(c(15, 20), c(0.2, 0.5), xout = seq(15, 20))$y,
#   approx(c(21, 40), c(0.5, 0.3), xout = seq(21, 39))$y,
#   rep(0.3, 60)
# )
# plot(sampled_fraction_vec)
#
# m4 <- fit_seeiqr(
#   daily_diffs, chains = 6, iter = 300,
#   seeiqr_model = seeiqr_model, sampled_fraction_vec = sampled_fraction_vec,
#   seeiqr_model = seeiqr_model)
#
# library(future)
