source("data-model-prep.R")

pars <- c(
  N = 5.1e6, D = 5, k1 = 1 / 5,
  k2 = 1, q = 0.05,
  r = 1, ur = 0.2, f1 = 1.0,
  start_decline = 15,
  end_decline = 22
)

pars[['r']] <- 0.1
pars[['ur']] <- 0.02
pars

m1 <- fit_seeiqr(
  daily_diffs, chains = 6, iter = 300,
  pars = pars, save_state_predictions = TRUE,
  seeiqr_model = seeiqr_model)
print(m1$fit, pars = c("R0", "f2", "phi"))

.m1 <- list("r = 0.1; ur = 0.02" = m1)
g_proj <- make_projection_plot(.m1) +
  facet_grid(rows = vars(Scenario))

R0 <- purrr::map_df(.m1, function(.x) {
  data.frame(theta = "R0b", value = .x$post$R0, stringsAsFactors = FALSE)
}, .id = "Scenario")
f2 <- purrr::map_df(.m1, function(.x) {
  data.frame(theta = "Fraction of contacts removed", value = 1 - .x$post$f2, stringsAsFactors = FALSE)
}, .id = "Scenario")
theta_df <- bind_rows(R0, f2) %>% as_tibble()
my_limits <- function(x) if (max(x) < 2) c(0, 1) else c(2.6, 3.5)
g_theta <- ggplot(theta_df, aes(value)) +
  facet_grid(Scenario~theta, scales = "free") +
  geom_histogram(bins = 50, fill = .hist_blue, alpha = .7, colour = "grey90", lwd = 0.15) +
  coord_cartesian(expand = FALSE, ylim = c(0, NA)) +
  ylab("") +
  scale_x_continuous(limits = my_limits) + xlab("Parameter value") + ylab("Density")
# ggsave(paste0("figs-ms/sampFrac-grid-theta-posteriors.png"),
#   width = 5, height = 7)

cowplot::plot_grid(g_proj, g_theta, align = "hv", axis = "bt", rel_widths = c(1.5, 2))














#
# ggsave(paste0("figs-ms/sampFrac-grid-theta-proj.png"), width = 8, height = 8)
