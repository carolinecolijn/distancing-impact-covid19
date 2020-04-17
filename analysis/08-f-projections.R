source(here::here("analysis/data-model-prep.R"))

sd_strength <- seq(0, 1, 0.2)[4:6] %>% purrr::set_names()
m_fs <- purrr::map(sd_strength, ~ {
  fit_seeiqr(
    daily_diffs, forecast_days = 90,
    fixed_f_forecast = .x,
    seeiqr_model = seeiqr_model, chains = 8, iter = 900
  )
})
saveRDS(m_fs, "data-generated/f-proj-fits.rds")

# If coming back:
m_fs <- readRDS("data-generated/f-proj-fits.rds")
purrr::walk(m_fs, ~ print(.x$fit, pars = c("R0", "f2", "phi")))

# Fewer iterations because just plotting 250 draws:
sd_strength2 <- seq(0.6, 1, 0.2) %>% purrr::set_names()
# future::plan(future::multisession)
# m_fs2 <- furrr::future_map(sd_strength2, ~ {
m_fs2 <- purrr::map(sd_strength2, ~ {
  fit_seeiqr(
    daily_diffs,
    seed = 1, forecast_days = 90,
    fixed_f_forecast = .x, save_state_predictions = TRUE,
    seeiqr_model = seeiqr_model, chains = 2, iter = 350
  )
})
# future::plan(future::sequential)
saveRDS(m_fs2, "data-generated/f-proj-fits2.rds")
m_fs2 <- readRDS("data-generated/f-proj-fits2.rds")
purrr::walk(m_fs2, ~ print(.x$fit, pars = c("R0", "f2", "phi")))

set.seed(13223567)
.draws <- sample(seq_len(350), 75)
prevalence <- purrr::map_dfr(m_fs2, get_prevalence, .id = "scenario", draws = .draws)

# Plots -----------------------------------------------------------------------

# Case count predictions:

.theme <- theme(title = element_text(size = rel(0.9))) +
  theme(strip.text.x = element_text(angle = 0, hjust = 0)) +
  theme(axis.title.x = element_blank())
.coord <- coord_cartesian(
  expand = FALSE, ylim = c(0, 150),
  xlim = c(lubridate::ymd("2020-03-01"), lubridate::ymd("2020-07-10"))
)

.m_fs <- m_fs

names(.m_fs) <- paste0("Contacts removed: ", sprintf("%.0f", (1 - sd_strength) * 100), "%")
names(.m_fs)

names(.m_fs) <- paste0("(", LETTERS[1:3], ") ", names(.m_fs))
names(.m_fs)
sc_order <- names(.m_fs)
g1 <- make_projection_plot(.m_fs, facet = TRUE, ncol = 3, sc_order = sc_order) +
  .theme + .coord

# Prevalence predictions:

prevalence$scenario2 <- paste0("Contacts removed: ", sprintf("%.0f", (1 - as.numeric(prevalence$scenario)) * 100), "%")
unique(prevalence$scenario2)

prevalence$scenario2_noletters <- prevalence$scenario2
prevalence$scenario2_noletters <- factor(prevalence$scenario2_noletters,
  levels = c("Contacts removed: 40%", "Contacts removed: 20%", "Contacts removed: 0%")
)

prevalence$scenario2 <- gsub("Contacts removed: 40%",
  "(D) Contacts removed: 40%", prevalence$scenario2)
prevalence$scenario2 <- gsub("Contacts removed: 20%",
  "(E) Contacts removed: 20%", prevalence$scenario2)
prevalence$scenario2 <- gsub("Contacts removed: 0%",
  "(F) Contacts removed: 0%", prevalence$scenario2)

unique(prevalence$scenario2)

.coord_prev <- coord_cartesian(
  expand = FALSE, ylim = c(0, 30000),
  xlim = c(lubridate::ymd_hms("2020-03-01 00:00:00"), lubridate::ymd_hms("2020-07-10 23:59:00"))
)

obj <- m_fs2[[1]] # just for last_day_obs
.start <- prevalence$start[1]
g_prev <- prevalence %>%
  ggplot(aes(day, prevalence, group = iterations)) +
  annotate("rect",
    xmin = .start + lubridate::ddays(obj$last_day_obs),
    xmax = .start + lubridate::ddays(obj$last_day_obs + 90),
    ymin = 0, ymax = Inf, fill = "grey95"
  ) +
  geom_line(alpha = 0.11, col = .hist_blue) +
  ylab("Modelled prevalence") +
  facet_wrap(~scenario2) +
  scale_y_continuous(labels = scales::comma) +
  .theme +
  .coord_prev

gg <- cowplot::plot_grid(g1, g_prev, nrow = 2, align = "hv", rel_heights = c(1, 3))
ggsave("figs-ms/f-projections2.png", width = 7, height = 7.25, dpi = 400)
ggsave("figs-ms/f-projections2.pdf", width = 7, height = 7.25, plot = gg)

.max <- filter(prevalence, scenario2_noletters == "Contacts removed: 0%") %>%
  pull(prevalence) %>%
  quantile(probs = 0.99)
g <- g_prev + coord_cartesian(
  expand = FALSE, ylim = c(0, .max),
  xlim = c(
    lubridate::ymd_hms("2020-03-01 00:00:00"),
    lubridate::ymd_hms("2020-07-11 23:59:00")
  )
) + facet_wrap(~scenario2_noletters)
ggsave("figs-ms/f-projections-taaaaaaaall.png", width = 5.5, height = 14)
ggsave("figs-ms/f-projections-taaaaaaaall.pdf", width = 5.5, height = 14)

