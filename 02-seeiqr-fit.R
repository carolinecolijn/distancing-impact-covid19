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

# Look at sample fraction scenarios -------------------------------------------

library(dplyr)
sf1 <- tidyr::expand_grid(sampled_fraction1 = c(0.05), sampled_fraction2 = c(0.1, 0.2, 0.3))
sf2 <- tidyr::expand_grid(sampled_fraction1 = c(0.1), sampled_fraction2 = c(0.2, 0.3, 0.4))
sf <- bind_rows(sf1, sf2)

library(future)
plan(multisession, workers = parallel::detectCores()/2)
m_sf <- furrr::future_pmap(sf, function(sampled_fraction1, sampled_fraction2) {
  fit_seeiqr(
    daily_diffs, chains = 1, iter = 400,
    sampled_fraction1 = sampled_fraction1, sampled_fraction2 = sampled_fraction2,
    seeiqr_model = seeiqr_model)
})
purrr::walk(m_sf, ~ print(.x$fit, pars = c("R0", "f2", "phi", "sampFrac2")))
plan(sequential)
names(m_sf) <- paste0("sampFrac1 = ", sf$sampled_fraction1, "\nsampFrac2 = ", sf$sampled_fraction2, "\n")
theme_set(ggsidekick::theme_sleek())
source("make_projection_plot.R")
make_projection_plot(m_sf, ylim = c(0, 100), facet = FALSE, outer_quantile = c(0.25, 0.75))
ggsave(paste0("figs/sampFrac-grid.png"), width = 7, height = 3.5)

R0 <- purrr::map_df(m_sf, function(.x) {
  data.frame(theta = "R0", value = .x$post$R0, stringsAsFactors = FALSE)
}, .id = "Scenario")
f2 <- purrr::map_df(m_sf, function(.x) {
  data.frame(theta = "f2", value = .x$post$f2, stringsAsFactors = FALSE)
}, .id = "Scenario")
theta_df <- bind_rows(R0, f2) %>% as_tibble()
my_limits <- function(x) if (max(x) < 2) c(0, 1) else c(2, 3)
ggplot(theta_df, aes(value)) +
  facet_grid(Scenario~theta, scales = "free") +
  geom_histogram(bins = 50, fill = "white", colour = "grey20") +
  coord_cartesian(expand = FALSE, ylim = c(0, NA)) +
  ylab("") +
  scale_x_continuous(limits = my_limits) + xlab("Parameter value") + ylab("Density")
ggsave(paste0("figs/sampFrac-grid-theta-posteriors.png"),
  width = 5, height = 7)

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
