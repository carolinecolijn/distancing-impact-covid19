source(here::here("analysis/data-model-prep.R"))

# Look at sample fraction scenarios -------------------------------------------

sf1 <- tidyr::expand_grid(
  sampled_fraction1 = c(0.05),
  sampled_fraction2 = c(0.1, 0.2, 0.3)
)
sf2 <- tidyr::expand_grid(
  sampled_fraction1 = c(0.1),
  sampled_fraction2 = c(0.2, 0.3, 0.4)
)
sf <- bind_rows(sf1, sf2)

m_sf <- purrr::pmap(sf, function(sampled_fraction1, sampled_fraction2) {
  fit_seeiqr(
    daily_diffs,
    chains = 5, iter = 300, save_state_predictions = TRUE,
    sampled_fraction1 = sampled_fraction1,
    sampled_fraction2 = sampled_fraction2,
    seeiqr_model = seeiqr_model
  )
})
saveRDS(m_sf, file = "data-generated/sf-fit.rds")
m_sf <- readRDS("data-generated/sf-fit.rds")

purrr::walk(m_sf, ~ print(.x$fit, pars = c("R0", "f2", "phi", "sampFrac2")))
names(m_sf) <- paste0(
  "sampFrac1 = ", sf$sampled_fraction1,
  "\nsampFrac2 = ", sf$sampled_fraction2
)

g_proj <- make_projection_plot(m_sf) +
  facet_grid(rows = vars(Scenario))

R0 <- purrr::map_df(m_sf, function(.x) {
  data.frame(theta = "R0b", value = .x$post$R0, stringsAsFactors = FALSE)
}, .id = "Scenario")
f2 <- purrr::map_df(m_sf, function(.x) {
  data.frame(
    theta = "Fraction of contacts removed",
    value = 1 - .x$post$f2, stringsAsFactors = FALSE
  )
}, .id = "Scenario")
theta_df <- bind_rows(R0, f2) %>% as_tibble()
my_limits <- function(x) if (max(x) < 2) c(0, 1) else c(2.6, 3.5)
g_theta <- ggplot(theta_df, aes(value)) +
  facet_grid(Scenario ~ theta, scales = "free") +
  geom_histogram(
    bins = 50, fill = .hist_blue,
    alpha = .7, colour = "grey90", lwd = 0.15
  ) +
  coord_cartesian(expand = FALSE, ylim = c(0, NA)) +
  ylab("") +
  scale_x_continuous(limits = my_limits) +
  xlab("Parameter value") +
  ylab("Density")

.start <- lubridate::ymd_hms("2020-03-01 00:00:00")
prevalence <- purrr::map_df(m_sf, get_prevalence, .id = "Scenario")

obj <- m_sf[[1]]
g_prev <- ggplot(prevalence, aes(day, prevalence, group = iterations)) +
  annotate("rect",
    xmin = .start + lubridate::ddays(obj$last_day_obs),
    xmax = .start + lubridate::ddays(obj$last_day_obs + 60), ymin = 0, ymax = Inf, fill = "grey95"
  ) +
  geom_line(alpha = 0.05, col = .hist_blue) +
  ylab("Modelled prevalence") +
  facet_grid(rows = vars(Scenario)) +
  coord_cartesian(expand = FALSE, xlim = c(.start, .start + lubridate::ddays(obj$last_day_obs + 60)), ylim = c(0, max(prevalence$prevalence) * 1.04)) +
  xlab("")
g_prev

g <- cowplot::plot_grid(g_prev, g_proj, g_theta,
  align = "hv",
  axis = "bt", rel_widths = c(1.2, 1.2, 2), ncol = 3
)

ggsave(paste0("figs-ms/sampFrac-grid-theta-proj.png"), width = 10, height = 8)
ggsave(paste0("figs-ms/sampFrac-grid-theta-proj.pdf"), width = 10, height = 8)

# More sensitivity tests --------------------------------------------------------------

# D=4, k1=1/4, 83% . --- rationale: shorter duration all round, R0 is lower,
# still consistent w data, still consistent message re strength of distancing
# D=6, k1=1/6, 83% --- rationale: longer duration all round, R0 is higher, still
# consistent w data & message about distancing D=5, k=1/5 (which I think are now
# our "default" main text parameters, BUT with 70% (r/(u+r) = 0.7) -- rationale:
# what if we were too optimistic about the portion doing the distancing? how
# sensitive are the results to this choice?

pars <- c(
  N = 5.1e6, D = 5, k1 = 1 / 5,
  k2 = 1, q = 0.05,
  r = 0.1, ur = 0.02, f1 = 1.0,
  start_decline = 15,
  end_decline = 22
)

pars1 <- pars
pars1[["D"]] <- 4
pars1[["k1"]] <- 1 / 4

m1 <- fit_seeiqr(
  daily_diffs, seed = 1256,
  chains = 6, iter = 300,
  pars = pars1, save_state_predictions = TRUE,
  seeiqr_model = seeiqr_model
)

pars2 <- pars
pars2[["D"]] <- 6
pars2[["k1"]] <- 1 / 6

m2 <- fit_seeiqr(
  daily_diffs,
  chains = 6, iter = 300,
  pars = pars2, save_state_predictions = TRUE,
  seeiqr_model = seeiqr_model
)

pars3 <- pars
pars3[["D"]] <- 5
pars3[["k1"]] <- 1 / 5
pars3[["ur"]] <- getu(f = 0.7, r = 0.1) # 70%

m3 <- fit_seeiqr(
  daily_diffs,
  chains = 6, iter = 300,
  pars = pars3, save_state_predictions = TRUE,
  seeiqr_model = seeiqr_model
)

m_sens <- list(m1, m2, m3)
purrr::walk(m_sens, ~ print(.x$fit, pars = c("R0", "f2", "phi")))
names(m_sens) <- c(
  "D = 4, k1 = 1/4, e = 0.83",
  "D = 6, k1 = 1/6, e = 0.83",
  "D = 5, k1 = 1/5, e = 0.70"
)

get_thresh <- function(.pars) {
  fs <- seq(0.25, 1, 0.25)
  m_fs <- purrr::map(fs, function(.f) {
    fit_seeiqr(
      daily_diffs,
      pars = .pars,
      iter = 300, chains = 1, save_state_predictions = TRUE,
      seeiqr_model = seeiqr_model, fixed_f_forecast = .f
    )
  })
  slopes <- purrr::map2_df(m_fs, fs, get_prevalence_slope)
  slopes$inverse_f <- 1 - slopes$f
  mlm <- lm(slope ~ inverse_f, data = slopes)
  nd <- data.frame(inverse_f = 1 - seq(0.3, 1, length.out = 5000))
  nd$predicted_slope <- predict(mlm, newdata = nd)
  thresh <- dplyr::filter(nd, predicted_slope > 0) %>% `[`(1, "inverse_f")
  thresh
}

plan(multisession, workers = parallel::detectCores() / 2)
thresholds <- furrr::future_map_dbl(list(pars1, pars2, pars3), get_thresh)
plan(future::sequential)

thresh_df <- tibble(Scenario = names(m_sens), threshold = thresholds)

R0 <- purrr::map_df(m_sens, function(.x) {
  data.frame(theta = "R0b", value = .x$post$R0, stringsAsFactors = FALSE)
}, .id = "Scenario")
f2 <- purrr::map_df(m_sens, function(.x) {
  data.frame(
    theta = "Fraction of contacts removed",
    value = 1 - .x$post$f2, stringsAsFactors = FALSE
  )
}, .id = "Scenario")
f2 <- left_join(f2, thresh_df)

theta_df <- bind_rows(R0, f2)
my_limits <- function(x) if (max(x) < 2) c(0, 1) else range(R0$value) * c(0.98, 1.02)

g_theta <- ggplot(theta_df, aes(value)) +
  facet_grid(Scenario ~ theta, scales = "free") +
  geom_histogram(bins = 50, fill = .hist_blue, alpha = .7, colour = "grey90", lwd = 0.15) +
  coord_cartesian(expand = FALSE, ylim = c(0, NA)) +
  ylab("") +
  scale_x_continuous(limits = my_limits) +
  xlab("Parameter value") +
  ylab("Density") +
  geom_vline(aes(xintercept = threshold), lty = 2, col = "grey50")

g_proj <- make_projection_plot(m_sens) +
  facet_grid(rows = vars(Scenario))

# prev:

.start <- lubridate::ymd_hms("2020-03-01 00:00:00")
prevalence <- purrr::map_df(m_sens, get_prevalence, .id = "Scenario")

obj <- m_sens[[1]]
g_prev <- ggplot(prevalence, aes(day, prevalence, group = iterations)) +
  annotate("rect",
    xmin = .start + lubridate::ddays(obj$last_day_obs),
    xmax = .start + lubridate::ddays(obj$last_day_obs + 60),
    ymin = 0, ymax = Inf, fill = "grey95"
  ) +
  geom_line(alpha = 0.05, col = .hist_blue) +
  ylab("Modelled prevalence") +
  facet_grid(rows = vars(Scenario)) +
  coord_cartesian(expand = FALSE,
    xlim = c(.start, .start + lubridate::ddays(obj$last_day_obs + 60)),
    ylim = c(0, max(prevalence$prevalence) * 1.04)) +
  xlab("")
# g_prev

g <- cowplot::plot_grid(g_prev, g_proj, g_theta,
  align = "hv",
  axis = "bt", rel_widths = c(1.2, 1.2, 2), ncol = 3
)

ggsave(paste0("figs-ms/sensitivity1-theta-proj.png"), width = 10, height = 6)
ggsave(paste0("figs-ms/sensitivity1-theta-proj.pdf"), width = 10, height = 6)
