source("data-model-prep.R")

load_tidy_delay_data <- function() {
  linelist_latest_file <-
    here::here("nCoVDailyData/linelist/2019-nCoV_daily_linelist.csv")
  if (!file.exists(linelist_latest_file)) {
    stop(paste(
      "You need to have the file ",
      linelist_latest_file,
      " ."
    ))
  }
  linelist <- read.csv(linelist_latest_file,
    stringsAsFactors = F,
    na.strings = ""
  )
  names(linelist)[1] <- "investigation_id" # else it seems to be "i..investigation_id"

  delay_data_with_outliers <- as_tibble(linelist) %>%
    select(reported_date, symptom_onset_date) %>%
    mutate_all(lubridate::ymd) %>%
    filter(!is.na(reported_date) & !is.na(symptom_onset_date)) %>%
    mutate(time_to_report = reported_date - symptom_onset_date)

  # Removing the following outliers that are $<0$ or $\geq 30$ days, since most
  #  likely to be data-entry errors, to yield the final dataset.
  # filter(delay_data_with_outliers,
  #     time_to_report >= 30 | time_to_report < 0)
  delay_data <- filter(
    delay_data_with_outliers,
    time_to_report < 30 & time_to_report >= 0
  )
  return(list(
    "delay_data" = delay_data,
    "delay_data_with_outliers" = delay_data_with_outliers
  ))
}

plot_time_to_report <- function(data,
                                start_date_report = "2020-02-29",
                                start_date_onset = "2020-02-29",
                                show_weekends = FALSE,
                                x_axis = "reported") {
  stopifnot(x_axis %in% c("reported", "onset"))

  if (x_axis == "reported") {
    p <- data %>%
      filter(reported_date > start_date_report) %>%
      mutate(weekday = if_else(lubridate::wday(reported_date) %in% c(7, 1),
        "Weekend",
        "Weekday"
      )) %>%
      ggplot(aes(
        x = reported_date,
        y = time_to_report
      )) +
      geom_jitter(height = 0, width = 0.2, alpha = 0.4, pch = 19, col = .hist_blue) +
      geom_boxplot(aes(group = reported_date),
        outlier.shape = NA, fill = NA, colour = "grey30", alpha = 0.6,
      ) +
      scale_fill_distiller(palette = "Blues", direction = 1, limits = c(1, 10)) +
      coord_cartesian(expand = FALSE) +
      labs(
        fill = "Reported\ncases",
        x = "Date of reported case",
        y = "Time from symptom onset\n to reported case (days)"
      )
    if (show_weekends) {
      p <- p + geom_boxplot(aes(
        group = reported_date,
        fill = weekday
      ))
    }
  }

  if (x_axis == "onset") {
    p <- data %>%
      filter(symptom_onset_date > start_date_onset) %>%
      mutate(weekday = if_else(lubridate::wday(reported_date) %in% c(7, 1),
        "Weekend",
        "Weekday"
      )) %>%
      ggplot(aes(
        x = symptom_onset_date,
        y = time_to_report
      )) +
      geom_jitter(height = 0, width = 0.22, alpha = 0.35, size = 0.9, pch = 19, col = .hist_blue) +
      geom_boxplot(aes(group = symptom_onset_date),
        outlier.shape = NA, fill = NA, colour = "grey30", alpha = 0.6,
      ) +
      scale_fill_distiller(palette = "Blues", direction = 1, limits = c(1, 10)) +
      geom_abline(
        slope = -1,
        intercept = lubridate::ymd(max(data$reported_date)),
        linetype = 2, col = "grey50"
      ) +
      labs(
        fill = "Reported\ncases",
        x = "Date of onset of symptoms",
        y = "Time from symptom onset\n to reported case (days)"
      )
    if (show_weekends) {
      p <- p + geom_boxplot(aes(
        group = symptom_onset_date,
        fill = weekday
      ))
    }
  }
  p + ggsidekick::theme_sleek() + coord_cartesian(expand = FALSE)
}

delay_data <- load_tidy_delay_data()[["delay_data"]]
# plotdelay1 <- plot_time_to_report(delay_data) + xlim(lubridate::ymd("2020-03-05"), lubridate::ymd("2020-04-06")) + guides(fill = FALSE) + theme(axis.title.y = element_blank())
plotdelay2 <- plot_time_to_report(delay_data, x_axis = "onset") + xlim(lubridate::ymd("2020-02-29"), lubridate::ymd("2020-04-06")) +
  coord_cartesian(expand = FALSE, ylim = c(0, 32))

start_date_report <- "2020-02-29"

.x <- delay_data %>%
  filter(reported_date > start_date_report) %>%
  group_by(reported_date) %>%
  summarize(confirmed_cases = n())

plotdelay1 <- .x %>%
  ggplot(aes(x = reported_date, y = confirmed_cases)) +
  geom_col(fill = .hist_blue, alpha = .8,
    colour = "grey90", lwd = 0.15) +
  coord_cartesian(expand = FALSE, ylim = c(0, max(.x$confirmed_cases) * 1.02)) +
  labs(
    fill = "Reported\ncases",
    x = "Date of reported case",
    y = "Reported cases"
  ) + xlim(lubridate::ymd("2020-02-29"), lubridate::ymd("2020-04-06"))


g <- cowplot::plot_grid(plotdelay1, plotdelay2, ncol = 1,
  labels = "AUTO", align = "hv",  label_x = 0.145, label_y = 0.962) +
  theme(plot.margin = margin(2, 11, 2, 0))
ggsave("figs-ms/onset.png", width = 4.5, height = 4.7, dpi = 400)

# y.grob <- grid::textGrob("Time from symptom onset to reported case (days)",
#   gp = grid::gpar(col = "grey30", fontsize = 11), rot = 90
# )

# png("figs-ms/onset.png", width = 4.2, height = 5.5, units = "in", res = 300)
# gridExtra::grid.arrange(gridExtra::arrangeGrob(g, left = y.grob, padding = unit(0.5, "line")))
# dev.off()
