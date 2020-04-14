setwd(here::here("selfIsolationModel/stan"))
source("data-model-prep.R")
dir.create("figs-cdc", showWarnings = FALSE)

days_change_sd <- length(seq(lubridate::ymd("2020-03-01"), lubridate::ymd("2020-05-01"), by = "1 day"))
days_change_sd
.start <- lubridate::ymd("2020-03-01")

m <- fit_seeiqr(daily_diffs, seeiqr_model = seeiqr_model, iter = 2000, chains = 8)
saveRDS(m, file = "data-generated/main-fit-2000.rds")
m <- readRDS("data-generated/main-fit-2000.rds")
print(m$fit, pars = c("R0", "f2", "phi"))

sd_strength <- seq(0, 1, 0.2) %>% purrr::set_names()
m_fs <- purrr::map(sd_strength, ~ {
  fit_seeiqr(
    daily_diffs,
    fixed_f_forecast = .x, day_start_fixed_f_forecast = days_change_sd,
    seeiqr_model = seeiqr_model, chains = 8, iter = 1600
  )
})
saveRDS(m_fs, "data-generated/f-proj-fits-cdc.rds")

# If coming back:
# m_fs <- readRDS("data-generated/f-proj-fits-cdc.rds")
m_fs <- readRDS("data-generated/f-proj-fits.rds") # April 12
purrr::walk(m_fs, ~ print(.x$fit, pars = c("R0", "f2", "phi", "sampFrac2")))
names(m_fs)

ylim <- c(0, 160)
ylim_c <- c(0, 4500)

.coord <- coord_cartesian(
  expand = FALSE, ylim = ylim,
  xlim = c(lubridate::ymd("2020-03-01"), lubridate::ymd("2020-06-10"))
)
.coord_c <- coord_cartesian(
  expand = FALSE, ylim = ylim_c,
  xlim = c(lubridate::ymd("2020-03-01"), lubridate::ymd("2020-06-10"))
)
.theme <- theme(title = element_text(size = rel(0.9))) +
  theme(strip.text.x = element_text(angle = 0, hjust = 0))

.vline <- geom_vline(xintercept = .start + days_change_sd - 1, lty = 2, alpha = 0.4)

.today
.m_fs <- m_fs
names(.m_fs) <- paste0("Physical distancing: ", sprintf(
  "%.0f",
  (1 - sd_strength) * 100
), "%")
names(.m_fs)
sc_order <- names(.m_fs)

g <- make_projection_plot(.m_fs, facet = TRUE, ncol = 2, sc_order = sc_order) +
  .theme + .coord #+ .vline
ggsave(paste0("figs-cdc/proj-facet-", .today, ".png"),
  width = 6, height = 6, dpi = 450
)

g <- make_projection_plot(.m_fs,
  ylim = ylim_c, facet = TRUE, ncol = 2,
  cumulative = TRUE, sc_order = sc_order
) + .theme + .coord_c #+ .vline
ggsave(paste0("figs-cdc/proj-cumulative-facet-", .today, ".png"),
  width = 6, height = 6, dpi = 450
)

make_one_panel <- function(obj, title) {
  make_projection_plot(list(obj), ylim = ylim, points_size = 1.6) +
    ggtitle(title) +
    .coord + .theme #+ .vline
  file_name <- gsub("%", "", gsub("[a-zA-Z :]", "", title))
  ggsave(paste0("figs-cdc/proj-", file_name, "-", .today, ".png"),
    width = 5, height = 3.25, dpi = 450
  )
}
make_one_panel_cumulative <- function(obj, title) {
  make_projection_plot(list(obj), ylim = ylim_c, points_size = 1.6, cumulative = TRUE) +
    ggtitle(title) +
    .coord_c + .theme# + .vline
  file_name <- gsub("%", "", gsub("[a-zA-Z :]", "", title))
  ggsave(paste0("figs-cdc/proj-cumulative-", file_name, "-", .today, ".png"),
    width = 5, height = 3.25, dpi = 450
  )
}
purrr::walk(seq_along(.m_fs), ~ make_one_panel(.m_fs[[.x]], names(.m_fs)[.x]))
purrr::walk(seq_along(.m_fs), ~ make_one_panel_cumulative(.m_fs[[.x]], names(.m_fs)[.x]))

sd_est <- sprintf("%.0f", 100 * (1 - round(quantile(m$post$f2, c(0.05, 0.5, 0.95)), 2)))
sd_text <- paste0(
  "Physical distancing: ",
  sd_est[[2]], "% (90% CI: ", sd_est[3], "–", sd_est[1], "%)"
)
sd_text

make_projection_plot(list(m), points_size = 1.6, ylim = ylim) +
  ggtitle(sd_text) +
  .coord + .theme
ggsave(paste0("figs-cdc/est-proj-", .today, ".png"), width = 5, height = 3.25, dpi = 450)

make_projection_plot(list(m), ylim = ylim_c, points_size = 1.6, cumulative = TRUE) +
  ggtitle(sd_text) +
  .coord_c + .theme
ggsave(paste0("figs-cdc/est-proj-cumulative-", .today, ".png"),
  width = 5, height = 3.25, dpi = 450
)

# CSV output ------------------------------------------------------------------

get_dat_output <- function(models, cumulative = FALSE, first_date = "2020-03-01") {
  obj <- models[[1]]
  actual_dates <- seq(lubridate::ymd(first_date),
    lubridate::ymd(first_date) + max(obj$days),
    by = "1 day"
  )
  out <- purrr::map_df(models, function(.x) {
    temp <- .x$post$y_rep %>%
      reshape2::melt() %>%
      dplyr::rename(day = Var2)
    if (cumulative) {
      temp <- temp %>%
        group_by(iterations) %>%
        mutate(value = cumsum(value)) %>%
        ungroup()
    }
    temp %>%
      group_by(day) %>%
      summarise(
        q0.05 = round(quantile(value, probs = 0.05)),
        q0.25 = round(quantile(value, probs = 0.25)),
        q0.5 = round(quantile(value, probs = 0.5)),
        q0.75 = round(quantile(value, probs = 0.75)),
        q0.95 = round(quantile(value, probs = 0.95)),
        mean = sprintf("%.1f", round(mean(value), 1))
      ) %>%
      mutate(forecast = day > obj$last_day_obs) %>%
      mutate(day = actual_dates[day]) %>%
      dplyr::filter(day <= lubridate::ymd("2020-06-08"))
  }, .id = "Scenario")
  out
}

get_dat_output(.m_fs) %>%
  readr::write_csv(paste0("figs-cdc/proj-60-", .today, ".csv"))
get_dat_output(.m_fs, cumulative = TRUE) %>%
  readr::write_csv(paste0("figs-cdc/proj-cumulative-60-", .today, ".csv"))

if (Sys.info()[["user"]] == "seananderson") {
  system("cp -r figs-cdc/ ~/Dropbox/bc-cdc-may1/")
}
