source("data-model-prep.R")

# D=4, k1=1/4, 83% . --- rationale: shorter duration all round, R0 is lower, still consistent w data, still consistent message re strength of distancing
# D=6, k1=1/6, 83% --- rationale: longer duration all round, R0 is higher, still consistent w data & message about distancing
# D=5, k=1/5 (which I think are now our "default" main text parameters, BUT with 70% (r/(u+r) = 0.7) -- rationale: what if we were too optimistic about the portion doing the distancing? how sensitive are the results to this choice?

# Look at sample fraction scenarios -------------------------------------------

sf1 <- tidyr::expand_grid(sampled_fraction1 = c(0.05), sampled_fraction2 = c(0.1, 0.2, 0.3))
sf2 <- tidyr::expand_grid(sampled_fraction1 = c(0.1), sampled_fraction2 = c(0.2, 0.3, 0.4))
sf <- bind_rows(sf1, sf2)

library(future)
plan(multisession, workers = parallel::detectCores())
m_sf <- furrr::future_pmap(sf, function(sampled_fraction1, sampled_fraction2) {
  fit_seeiqr(
    daily_diffs, chains = 1, iter = 1000,
    sampled_fraction1 = sampled_fraction1, sampled_fraction2 = sampled_fraction2,
    seeiqr_model = seeiqr_model)
})
plan(sequential)
saveRDS(m_sf, file = "data-generated/sf-fit.rds")
m_sf <- readRDS("data-generated/sf-fit.rds")

purrr::walk(m_sf, ~ print(.x$fit, pars = c("R0", "f2", "phi", "sampFrac2")))
names(m_sf) <- paste0("sampFrac1 = ", sf$sampled_fraction1, "\nsampFrac2 = ", sf$sampled_fraction2, "\n")

make_projection_plot(m_sf, ylim = c(0, 100), facet = FALSE, outer_quantile = c(0.25, 0.75),
  cols = RColorBrewer::brewer.pal(8, "Dark2"))
ggsave(paste0("figs-ms/sampFrac-grid.png"), width = 7, height = 3.5)

names(m_sf) <- paste0("sampFrac1 = ", sf$sampled_fraction1, "\nsampFrac2 = ", sf$sampled_fraction2)
R0 <- purrr::map_df(m_sf, function(.x) {
  data.frame(theta = "R0", value = .x$post$R0, stringsAsFactors = FALSE)
}, .id = "Scenario")
f2 <- purrr::map_df(m_sf, function(.x) {
  data.frame(theta = "Contact parameter", value = 1 - .x$post$f2, stringsAsFactors = FALSE)
}, .id = "Scenario")
theta_df <- bind_rows(R0, f2) %>% as_tibble()
my_limits <- function(x) if (max(x) < 2) c(0, 1) else c(2.6, 3.5)
ggplot(theta_df, aes(value)) +
  facet_grid(Scenario~theta, scales = "free") +
  geom_histogram(bins = 50, fill = .hist_blue, alpha = .7, colour = "grey90", lwd = 0.15) +
  coord_cartesian(expand = FALSE, ylim = c(0, NA)) +
  ylab("") +
  scale_x_continuous(limits = my_limits) + xlab("Parameter value") + ylab("Density")
ggsave(paste0("figs-ms/sampFrac-grid-theta-posteriors.png"),
  width = 5, height = 7)

# Sensitivity to k1 and D -----------------------------------------------------
#
# m_slower <- fit_seeiqr(
#   daily_diffs, chains = 8, iter = 300,
#   pars = c(
#     N = 4.4e6, D = 6, k1 = 1 / 5,
#     k2 = 1, q = 0.05,
#     r = 1, ur = 0.2, f1 = 1.0,
#     start_decline = 15,
#     end_decline = 22
#   ), save_state_predictions = TRUE,
#   seeiqr_model = seeiqr_model)
# saveRDS(m_slower, file = "data-generated/m-slower.rds")
# m_slower <- readRDS("data-generated/m-slower.rds")
#
# make_projection_plot(list(m_slower))
#
# source("make_quick_plots.R")
# make_quick_plots(m_slower, id = "slower")
