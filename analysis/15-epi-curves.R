source(here::here("analysis/data-model-prep.R"))

# m <- fit_seeiqr(daily_diffs, seeiqr_model = seeiqr_model, iter = 2000, chains = 8)
# print(m$fit, pars = c("R0", "f2", "phi"))
# saveRDS(m, file = "data-generated/main-fit-2000.rds")
m <- readRDS("data-generated/main-fit-2000.rds")

.last_day <- m$last_day_obs
.last_day

sdtiming0.7 <- function(t, start_decline = 15, end_decline = 22,
                        last_obs = .last_day,
                        f_val = 0.7,
                        f1 = pars$f1,
                        f2 = pars$f2) {
  if (t < start_decline) {
    return(f1)
  }
  if (t >= start_decline & t < end_decline) {
    return(f2 + (end_decline - t) * (f1 - f2) / (end_decline - start_decline))
  }
  floor_t <- floor(t)
  if (t >= end_decline & floor_t <= last_obs) {
    return(f2)
  }
  if (t >= end_decline & floor_t > last_obs) {
    return(f_val)
  }
}

sdtiming0.8 <- sdtiming0.7
formals(sdtiming0.8)$f_val <- 0.8

sdtiming0.9 <- sdtiming0.7
formals(sdtiming0.9)$f_val <- 0.9

sdtiming1.0 <- sdtiming0.7
formals(sdtiming1.0)$f_val <- 1.0

sdtiming0.6 <- sdtiming0.7
formals(sdtiming0.6)$f_val <- 0.6

proj_wrapper <- function(.sdfunction, .n = 20, .proj_days = 500) {
  proj_days <- .last_day + .proj_days
  .times <- seq(-30, proj_days, 0.1)
  list(m$post$R0[1:.n], m$post$f2[1:.n], m$post$phi[1:.n], seq_len(.n)) %>%
    purrr::pmap_dfr(reproject_fits,
      obj = m, .sdfunc = .sdfunction,
      .time = .times, return_ode_dat = TRUE
    ) %>%
    dplyr::mutate(prevalence = I + Id) %>%
    dplyr::as_tibble()
}

sdfuncs <- list(sdtiming0.6, sdtiming0.7, sdtiming0.8, sdtiming0.9, sdtiming1.0) %>%
  purrr::set_names(seq(0.6, 1.0, 0.1))

plan(multisession)
prev <- sdfuncs %>% furrr::future_map_dfr(proj_wrapper,
  .id = "scenario",
  .options = furrr::future_options(
    globals =
      c(
        ".last_day", "m", "sdtiming0.6", "sdtiming0.7",
        "sdtiming0.8", "reproject_fits",
        "sdtiming0.9", "sdtiming1.0", "socdistmodel", "getlambd"
      )
  )
)
plan(sequential)
saveRDS(prev, file = "data-generated/prev-epi.rds")
prev <- readRDS("data-generated/prev-epi.rds")

.start <- lubridate::ymd_hms("2020-03-01 00:00:00")
prev <- mutate(prev, day = .start + lubridate::ddays(time))

obj <- m # just for last_day_obs
.theme <- theme(title = element_text(size = rel(0.9))) +
  theme(strip.text.x = element_text(angle = 0, hjust = 0)) +
  theme(axis.title.x = element_blank())

.coord_prev <- coord_cartesian(
  expand = FALSE, ylim = c(0, NA),
  xlim = c(lubridate::ymd_hms("2020-03-01 00:00:00"), NA)
)

g <- prev %>%
  mutate(scenario2 = paste0(100 * (as.numeric(scenario)), "%")) %>%
  mutate(scenario2 = factor(scenario2, levels = c("60%", "70%", "80%", "90%", "100%"))) %>%
  ggplot(aes(day, prevalence,
    group = paste(iterations, scenario2), colour = scenario2
  )) +
  annotate("rect",
    xmin = .start + lubridate::ddays(obj$last_day_obs),
    xmax = .start + lubridate::ddays(obj$last_day_obs + 500),
    ymin = 0, ymax = Inf, fill = "grey96"
  ) +
  labs(colour = "Fraction of\nnormal contacts") +
  geom_line(alpha = 0.35) +
  ylab("Modelled prevalence") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, lwd = 1))) +
  scale_color_viridis_d(direction = 1) +
  .theme +
  .coord_prev +
  theme(legend.position = c(0.82, 0.77)) +
  scale_y_continuous(labels = scales::comma) +
  annotate("text", x = lubridate::ymd_hms("2020-03-22 00:00:00"), y = 80000, label = "See inset", angle = 90, col = "grey30", size = 3.5) +
  scale_x_datetime(date_breaks = "4 months", date_labels = "%b %Y")

# ggsave("figs-ms/epi-curves.png", width = 5, height = 3.25)

g3 <- g + theme_void() +
  theme(panel.border = element_rect(fill = NA, colour = "grey70", size = 1)) +
  coord_cartesian(
  expand = FALSE, ylim = c(0, 3000),
  xlim = c(lubridate::ymd_hms("2020-02-27 00:00:00"), lubridate::ymd_hms("2020-07-01 00:00:00"))) +
    guides(colour = FALSE)
plot_with_inset <-
  cowplot::ggdraw() +
  cowplot::draw_plot(g) +
  cowplot::draw_plot(g3, x = .7, y = .15, width = .25, height = .25)

ggsave(
  filename = "figs-ms/epi-curves-inset.png",
  plot = plot_with_inset,
  width = 5, height = 3.25, dpi = 400)

ggsave(
  filename = "figs-ms/epi-curves-inset.pdf",
  plot = plot_with_inset,
  width = 5, height = 3.25)

# g2 <- g + scale_y_sqrt(labels = scales::comma)
# ggsave("figs-ms/epi-curves-sqrt.png", width = 4.5, height = 3.25)
