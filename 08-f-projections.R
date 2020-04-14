source("data-model-prep.R")

sd_strength <- seq(0, 1, 0.2) %>% purrr::set_names()
m_fs <- purrr::map(sd_strength, ~ {
  fit_seeiqr(
    daily_diffs,
    fixed_f_forecast = .x,
    seeiqr_model = seeiqr_model, chains = 8, iter = 2000
  )
})
saveRDS(m_fs, "data-generated/f-proj-fits.rds")

# If coming back:
m_fs <- readRDS("data-generated/f-proj-fits.rds")
purrr::walk(m_fs, ~ print(.x$fit, pars = c("R0", "f2", "phi")))

# Fewer iterations because just plotting 250 draws:
sd_strength2 <- seq(0.6, 1, 0.2) %>% purrr::set_names()
future::plan(future::multisession)
m_fs2 <- furrr::future_map(sd_strength2, ~ {
  fit_seeiqr(
    daily_diffs, seed = 1,
    fixed_f_forecast = .x, save_state_predictions = TRUE,
    seeiqr_model = seeiqr_model, chains = 1, iter = 500
  )
})
future::plan(future::sequential)
purrr::walk(m_fs2, ~ print(.x$fit, pars = c("R0", "f2", "phi")))

get_prevalence <- function(obj, draws = 1:250,
  .start = lubridate::ymd_hms("2020-03-01 00:00:00")) {
  post <- obj$post
  ts_df <- dplyr::tibble(time = obj$time, time_num = seq_along(obj$time))
  variables_df <- dplyr::tibble(
    variable = names(obj$state_0),
    variable_num = seq_along(obj$state_0)
  )
  ts_df <- dplyr::tibble(time = obj$time, time_num = seq_along(obj$time))
  states <- reshape2::melt(post$y_hat) %>%
    dplyr::rename(time_num = Var2, variable_num = Var3) %>%
    dplyr::filter(iterations %in% draws) %>%
    dplyr::left_join(variables_df, by = "variable_num") %>%
    dplyr::left_join(ts_df, by = "time_num")
  prevalence <- states %>%
    dplyr::filter(variable %in% c("I", "Id")) %>%
    group_by(iterations, time) %>%
    summarize(I = value[variable == "I"], Id = value[variable == "Id"],
      prevalence = I + Id) %>%
    mutate(day = .start + lubridate::ddays(time), start = .start)
  prevalence
}

prevalence <- purrr::map_dfr(m_fs2, get_prevalence, .id = "scenario")

# Plots -----------------------------------------------------------------------

# Case count predictions:

.theme <- theme(title = element_text(size = rel(0.9))) +
  theme(strip.text.x = element_text(angle = 0, hjust = 0)) +
  theme(axis.title.x = element_blank())
.coord <- coord_cartesian(
  expand = FALSE, ylim = c(0, 150),
  xlim = c(lubridate::ymd("2020-03-01"), lubridate::ymd("2020-06-10"))
)

.m_fs <- m_fs

names(.m_fs) <- paste0("Physical distancing: ", sprintf("%.0f", (1 - sd_strength) * 100), "%")
names(.m_fs)

.m_fs <- .m_fs[c(4, 5, 6)]

names(.m_fs) <- paste0("(", LETTERS[1:3], ") ", names(.m_fs) )
names(.m_fs)
sc_order <- names(.m_fs)
g1 <- make_projection_plot(.m_fs, facet = TRUE, ncol = 3, sc_order = sc_order) +
  .theme + .coord #+ theme(axis.text.x.bottom = element_blank())

# names(m_bccdc) <- paste0("Contact fraction: ", sprintf("%.1f", sd_strength))

# Prevalence predictions:

prevalence$scenario2 <- paste0("Physical distancing: ", sprintf("%.0f", (1 - as.numeric(prevalence$scenario)) * 100), "%")
unique(prevalence$scenario2)

prevalence$scenario2_noletters <- prevalence$scenario2
prevalence$scenario2_noletters <- factor(prevalence$scenario2_noletters,
  levels = c("Physical distancing: 40%", "Physical distancing: 20%", "Physical distancing: 0%"
))

prevalence$scenario2 <- gsub("Physical distancing: 40%", "(D) Physical distancing: 40%", prevalence$scenario2)
prevalence$scenario2 <- gsub("Physical distancing: 20%", "(E) Physical distancing: 20%", prevalence$scenario2)
prevalence$scenario2 <- gsub("Physical distancing: 0%", "(F) Physical distancing: 0%", prevalence$scenario2)

# prevalence$scenario2 <- factor(prevalence$scenario2, levels = sc_order)
unique(prevalence$scenario2)

.coord_prev <- coord_cartesian(
  expand = FALSE, ylim = c(0, 2200),
  xlim = c(lubridate::ymd_hms("2020-03-01 00:00:00"), lubridate::ymd_hms("2020-06-10 23:59:00"))
)

obj <- m_fs2[[1]] # just for last_day_obs
.start <- prevalence$start[1]
g_prev <- prevalence %>%
  ggplot(aes(day, prevalence, group = iterations)) +
  annotate("rect", xmin = .start + lubridate::ddays(obj$last_day_obs),
    xmax = .start + lubridate::ddays(obj$last_day_obs + 65),
    ymin = 0, ymax = Inf, fill = "grey95") +
  geom_line(alpha = 0.05, col = .hist_blue) +
  ylab("Prevalence") +
  facet_wrap(~scenario2) +
  .theme + .coord_prev
# g_prev

gg <- cowplot::plot_grid(g1, g_prev, nrow = 2, align = "hv")
ggsave("figs-ms/f-projections.png", width = 7, height = 4)

.max <- filter(prevalence, scenario2_noletters == "Physical distancing: 0%") %>%
  pull(prevalence) %>% quantile(probs = 0.99)
g <- g_prev + coord_cartesian(
  expand = FALSE, ylim = c(0, .max),
  xlim = c(lubridate::ymd_hms("2020-03-01 00:00:00"),
    lubridate::ymd_hms("2020-06-11 23:59:00"))
) + facet_wrap(~scenario2_noletters) +
  ggsave("figs-ms/f-projections-taaaaaaaall.png", width = 5.5, height = 14)
