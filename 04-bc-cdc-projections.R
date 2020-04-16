setwd(here::here("selfIsolationModel/stan"))
source("data-model-prep.R")

dat <- readr::read_csv(here::here("nCoVDailyData/CaseCounts/BC Case Counts.csv"))
names(dat)[names(dat) == "BC"] <- "Cases"
dat$Date <- lubridate::dmy(dat$Date)
dat$day <- seq_len(nrow(dat))
dat$daily_diffs <- c(
  dat$Cases[2] - dat$Cases[1],
  diff(dat$Cases)
)
.today <- max(dat$Date)
dat <- dplyr::filter(dat, Date >= "2020-03-01")
daily_diffs <- c(dat$daily_diffs, 44)

# id <- "2020-04-14-may1"
id <- "2020-04-15-april16"
.day_change <- lubridate::ymd("2020-04-16")
.start <- lubridate::ymd("2020-03-01")
dir.create(paste0("figs-cdc-", id), showWarnings = FALSE)

# days_change_sd <- length(seq(lubridate::ymd("2020-03-01"), lubridate::ymd("2020-05-01"), by = "1 day"))
# days_change_sd

days_change_sd <- length(seq(.start, .day_change, by = "1 day"))
days_change_sd

m <- fit_seeiqr(daily_diffs, seeiqr_model = seeiqr_model, iter = 1000, chains = 8)
# saveRDS(m, file = paste0("data-generated/main-fit-2000-", id, ".rds"))
# m <- readRDS(paste0("data-generated/main-fit-2000-", id, ".rds"))
print(m$fit, pars = c("R0", "f2", "phi"))

sd_strength <- seq(0, 1, 0.2) %>% purrr::set_names()
m_fs <- purrr::map(sd_strength, ~ {
  fit_seeiqr(
    daily_diffs,
    fixed_f_forecast = .x, day_start_fixed_f_forecast = days_change_sd,
    seeiqr_model = seeiqr_model, chains = 8, iter = 800
  )
})
saveRDS(m_fs, paste0("data-generated/f-proj-fits-cdc-", id, ".rds"))
m_fs <- readRDS(paste0("data-generated/f-proj-fits-cdc-", id, ".rds"))
# purrr::walk(m_fs, ~ print(.x$fit, pars = c("R0", "f2", "phi", "sampFrac2")))

ylim <- c(0, 160)
ylim_c <- c(0, 4500)

.coord <- coord_cartesian(
  expand = FALSE, ylim = ylim,
  xlim = c(lubridate::ymd("2020-03-01"), .today + 60)
)
.coord_c <- coord_cartesian(
  expand = FALSE, ylim = ylim_c,
  xlim = c(lubridate::ymd("2020-03-01"), .today + 60)
)
.theme <- theme(title = element_text(size = rel(0.9))) +
  theme(strip.text.x = element_text(angle = 0, hjust = 0))

.vline <- geom_vline(xintercept = .start + days_change_sd - 1, lty = 2, alpha = 0.4)

.m_fs <- m_fs
names(.m_fs) <- paste0("Percentage of contacts removed: ", sprintf("%.0f", (1 - sd_strength) * 100), "%")
names(.m_fs)
sc_order <- names(.m_fs)

g <- make_projection_plot(.m_fs, facet = TRUE, ncol = 2, sc_order = sc_order) +
  .theme + .coord #+ .vline
ggsave(paste0("figs-cdc-", id, "/proj-facet-", .today, ".png"),
  width = 6, height = 6, dpi = 450
)

g <- make_projection_plot(.m_fs,
  ylim = ylim_c, facet = TRUE, ncol = 2,
  cumulative = TRUE, sc_order = sc_order
) + .theme + .coord_c #+ .vline
ggsave(paste0("figs-cdc-", id, "/proj-cumulative-facet-", .today, ".png"),
  width = 6, height = 6, dpi = 450
)

make_one_panel <- function(obj, title) {
  make_projection_plot(list(obj), ylim = ylim, points_size = 1.6) +
    ggtitle(title) +
    .coord + .theme #+ .vline
  file_name <- gsub("%", "", gsub("[a-zA-Z :]", "", title))
  ggsave(paste0("figs-cdc-", id, "/proj-", file_name, "-", .today, ".png"),
    width = 5, height = 3.25, dpi = 450
  )
}
make_one_panel_cumulative <- function(obj, title) {
  make_projection_plot(list(obj), ylim = ylim_c, points_size = 1.6, cumulative = TRUE) +
    ggtitle(title) +
    .coord_c + .theme # + .vline
  file_name <- gsub("%", "", gsub("[a-zA-Z :]", "", title))
  ggsave(paste0("figs-cdc-", id, "/proj-cumulative-", file_name, "-", .today, ".png"),
    width = 5, height = 3.25, dpi = 450
  )
}
purrr::walk(seq_along(.m_fs), ~ make_one_panel(.m_fs[[.x]], names(.m_fs)[.x]))
purrr::walk(seq_along(.m_fs), ~ make_one_panel_cumulative(.m_fs[[.x]], names(.m_fs)[.x]))

sd_est <- sprintf("%.0f", 100 * (1 - round(quantile(m$post$f2, c(0.05, 0.5, 0.95)), 2)))
sd_text <- paste0(
  "Percentage of contacts removed: ",
  sd_est[[2]], "% (90% CI: ", sd_est[3], "–", sd_est[1], "%)"
)
sd_text

make_projection_plot(list(m), points_size = 1.6, ylim = ylim) +
  ggtitle(sd_text) +
  .coord + .theme
ggsave(paste0("figs-cdc-", id, "/est-proj-", .today, ".png"), width = 5, height = 3.25, dpi = 450)

make_projection_plot(list(m), ylim = ylim_c, points_size = 1.6, cumulative = TRUE) +
  ggtitle(sd_text) +
  .coord_c + .theme
ggsave(paste0("figs-cdc-", id, "/est-proj-cumulative-", .today, ".png"),
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
      dplyr::filter(day <= .today + 60)
  }, .id = "Scenario")
  out
}

get_dat_output(.m_fs) %>%
  readr::write_csv(paste0("figs-cdc-", id, "/proj-60-", .today, ".csv"))
get_dat_output(.m_fs, cumulative = TRUE) %>%
  readr::write_csv(paste0("figs-cdc-", id, "/proj-cumulative-60-", .today, ".csv"))

if (Sys.info()[["user"]] == "seananderson") {
  system(paste0("cp -r figs-cdc-", id, "/ ~/Dropbox/bc-cdc-", id, "/"))
  system(paste0("open ~/Dropbox/bc-cdc-", id, "/"))
}
