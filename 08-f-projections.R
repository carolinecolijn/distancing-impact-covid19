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

# .N <- 20
# select_fs <- c("0.6", "0.8", "1") %>% purrr::set_names()
#
# .last_day <- m_fs[[1]]$last_day_obs
# sdtiming_fixed_0.6 <- function(
#   t, start_decline = 15, end_decline = 22, last_obs = .last_day,
#   f_fixed = 0.6,
#   f1 = pars$f1,
#   f2 = pars$f2) {
#   if (t < start_decline) {
#     return(f1)
#   }
#   if (t >= start_decline & t < end_decline) {
#     return(f2 + (end_decline - t) * (f1 - f2) / (end_decline - start_decline))
#   }
#   floor_t <- floor(t)
#   if (t >= end_decline & floor_t <= last_obs) {
#     return(f2)
#   }
#   if (t >= end_decline & floor_t > last_obs) {
#     return(f_vec[floor_t])
#   }
# }
# sdtiming_fixed_0.8 <- sdtiming_fixed_0.6
# formals(sdtiming_fixed_0.8)$f_fixed <- 0.8
# sdtiming_fixed_1.0 <- sdtiming_fixed_0.6
# formals(sdtiming_fixed_1.0)$f_fixed <- 1.0
#
# future::plan(future::multisession, workers = parallel::detectCores() /2)
# d_ode <- furrr::future_map_dfr(select_fs, function(.x) {
#   m <- m_fs[[.x]]
#   list(m$post$R0[1:.N], m$post$f2[1:.N], m$post$phi[1:.N], 1:.N) %>%
#     purrr::pmap_dfr(reproject_fits, obj = m, .sdfunc = sdtiming_fixed,
#       return_ode_dat = TRUE)
# }, .id = "scenario")
# future::plan(future::sequential)

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

# m <- readRDS("data-generated/main-fit-500.rds")
prevalence <- purrr::map_dfr(m_fs2, get_prevalence, .id = "scenario")
# prevalence <- purrr::map_dfr(list(m), get_prevalence, .id = "scenario")

# make_projection_plot(m_fs2, ylim = c(0, 200))

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
  # coord_cartesian(expand = FALSE,
  #   xlim = c(.start, .start + lubridate::ddays(obj$last_day_obs + 60)),
  #   ylim = c(0, 2500)) +
  .theme + .coord_prev
# g_prev

gg <- cowplot::plot_grid(g1, g_prev, nrow = 2, align = "hv")
ggsave("figs-ms/f-projections.png", width = 7, height = 4)
