source(here::here("analysis/data-model-prep.R"))

.last_day <- lubridate::ymd("2020-04-12")

load_tidy_delay_data <- function() {
  # can't be publicly released
  linelist_latest_file <-
    here::here("data-raw/2019-nCoV_daily_linelist.csv")
  if (!file.exists(linelist_latest_file)) {
    stop(paste(
      "You need to have the file ",
      linelist_latest_file,
      " ."
    ))
  }
  linelist <- read.csv(linelist_latest_file,
    stringsAsFactors = FALSE,
    na.strings = ""
  )
  names(linelist)[1] <- "investigation_id"

  delay_data_with_outliers <- as_tibble(linelist) %>%
    select(reported_date, symptom_onset_date) %>%
    mutate_all(lubridate::ymd) %>%
    filter(!is.na(reported_date) & !is.na(symptom_onset_date)) %>%
    mutate(time_to_report = reported_date - symptom_onset_date)

  # Removing the following outliers that are $<0$ or $\geq 30$ days, since most
  # likely to be data-entry errors, to yield the final dataset.
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
                                show_weekends = FALSE) {

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
  p + coord_cartesian(expand = FALSE)
}

delay_data <- load_tidy_delay_data()[["delay_data"]]
plotdelay2 <- plot_time_to_report(delay_data) +
  xlim(lubridate::ymd("2020-02-29"), .last_day) +
  coord_cartesian(expand = FALSE, ylim = c(0, 32))

start_date_report <- "2020-02-29"

dat2 <- select(dat, Date, daily_diffs)
if (max(dat2$Date) == "2020-04-08") {
  dat2 <- data.frame(
    Date = c(dat2$Date, max(dat2$Date) + 1:3),
    daily_diffs = daily_diffs
  )
}

daily_diff_plot <- dat2 %>%
  ggplot(aes(x = Date, y = daily_diffs)) +
  geom_col(
    fill = .hist_blue, alpha = .8,
    colour = "grey90", lwd = 0.15
  ) +
  coord_cartesian(expand = FALSE,
    ylim = c(0, max(dat2$daily_diffs) * 1.02)) +
  labs(
    fill = "Reported\ncases",
    x = "Date of reported case",
    y = "New reported cases"
  ) +
  xlim(lubridate::ymd("2020-02-29"), .last_day)

# Hospital ----------------------------------------------------------

h <- readr::read_csv(here::here("data-raw/hospitalization-data.csv"))
h <- mutate(h, Date = lubridate::dmy(Date))
h <- bind_rows(
  select(h, date = Date, Count = `Hosp Census`) %>% mutate(Type = "Hospital"),
  select(dat, date = Date, Count = `ICU Census`) %>% mutate(Type = "ICU")
)
hosp_plot <- ggplot(h, aes(date, Count, colour = Type)) +
  geom_point(aes(shape = Type)) +
  geom_line() +
  coord_cartesian(
    expand = FALSE, ylim = c(0, 160),
    xlim = c(lubridate::ymd("2020-02-29"), .last_day)
  ) +
  theme(
    axis.title.x.bottom = element_blank(),
    legend.position = c(0.17, 0.25),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 11)
  ) +
  scale_color_manual(values = c(.hist_blue, "grey45")) +
  scale_shape_manual(values = c(21, 19)) +
  ylab("Census count") +
  labs(colour = "Census type", shape = "Census type")


g <- cowplot::plot_grid(daily_diff_plot, plotdelay2, hosp_plot,
  ncol = 1,
  labels = "AUTO", align = "hv", label_x = 0.18, label_y = 0.962
) +
  theme(plot.margin = margin(2, 11, 2, 0))
ggsave("figs-ms/onset-hosp.png", width = 4.1, height = 6.8, dpi = 350)
ggsave("figs-ms/onset-hosp.pdf", width = 4.1, height = 6.8)
