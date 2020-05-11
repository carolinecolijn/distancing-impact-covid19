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
      geom_jitter(height = 0, width = 0.22, alpha = 0.35, size = 0.8, pch = 19, col = .hist_blue) +
      geom_boxplot(aes(group = symptom_onset_date),
        outlier.shape = NA, fill = NA, colour = "grey40", alpha = 0.6, size = 0.5, coef = 0
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

ymd <- lubridate::ymd
#
#
# make_seg <- function(.date, .end = 1, .text = "") {
#   list(
#   geom_segment(x = lubridate::ymd(.date),
#     xend = lubridate::ymd(.date), y = 0, yend = 95 + .end * 3.5 , col = "grey60", lwd = 0.5),
#     annotate("text", x = lubridate::ymd(.date) + 0.3, y = 95 + .end * 3.5, label = .text, hjust = 0, col = "grey30", size = 3.5)
#   )
# }
# .xlim <- c(lubridate::ymd("2020-02-29"), .last_day)
# # daily_diff_plot <-
#   dat2 %>%
#   ggplot(aes(x = Date, y = daily_diffs)) +
#   geom_col(
#     fill = .hist_blue, alpha = .8,
#     colour = "grey90", lwd = 0.15
#   ) +
#   annotate(geom = "segment", y = Inf, yend = Inf, x = .xlim[1], xend = .xlim[2], colour = "grey70") +
#   annotate(geom = "segment", y = 0, yend = 0, x = .xlim[1], xend = .xlim[2], colour = "grey70") +
#   annotate(geom = "segment", y = -Inf, yend = Inf, x = .xlim[2], xend = .xlim[2], colour = "grey70") +
#   annotate(geom = "segment", y = -Inf, yend = Inf, x = .xlim[1], xend = .xlim[1], colour = "grey70") +
#   coord_cartesian(expand = FALSE,
#     ylim = c(0, max(dat2$daily_diffs) * 1.02), clip = "off") +
#   labs(
#     fill = "Reported\ncases",
#     x = "Date of reported case",
#     y = "New reported cases"
#   ) +
#   xlim(lubridate::ymd("2020-02-29"), .last_day) +
#   make_seg("2020-03-08", 8, "Testing") +
#   make_seg("2020-03-12", 7, "Testing") +
#   make_seg("2020-03-13", 6, "Testing") +
#   make_seg("2020-03-14", 5, "Testing") +
#   make_seg("2020-03-16", 4, "Testing") +
#   make_seg("2020-03-17", 3, "Testing") +
#   make_seg("2020-03-20", 2, "Testing") +
#   make_seg("2020-03-21", 1, "Testing") +
#
#     # geom_segment(x = ymd("2020-03-08"),
#     # xend = ymd("2020-03-08"), y = 0, yend = 100, col = "grey50") +
#     #
#     # geom_segment(x = ymd("2020-03-12"),
#     #   xend = ymd("2020-03-12"), y = 0, yend = 100, col = "grey50") +
#     #
#     # geom_segment(x = ymd("2020-03-13"),
#     #   xend = ymd("2020-03-13"), y = 0, yend = 100, col = "grey50") +
#     #
#     # geom_segment(x = ymd("2020-03-14"),
#     #   xend = ymd("2020-03-14"), y = 0, yend = 100, col = "grey50") +
#     #
#     # geom_segment(x = ymd("2020-03-16"),
#     #   xend = ymd("2020-03-16"), y = 0, yend = 100, col = "grey50") +
#
#   theme(plot.margin = margin(t = 90, r = 11/2, b = 11/2, l = 11/2), panel.border = element_blank())

d <- readr::read_csv(here::here("data-raw/timeline.csv"), comment = "#")
d <- d[-1, ]

daily_diff_plot <-
  dat2 %>%
  ggplot(aes(x = Date, y = daily_diffs)) +
  geom_rect(xmin = min(d$date), xmax = max(d$date), ymin  = 0, ymax = Inf, fill = "grey82", col = NA) +
  geom_col(
    fill = .hist_blue, alpha = .8,
    colour = "grey90", lwd = 0.15
  ) +
  # geom_rect(data = d, mapping = aes(xintercept = date), lty = 1, col = "grey55", lwd = 0.4) +
  coord_cartesian(expand = FALSE,
    ylim = c(0, max(dat2$daily_diffs) * 1.02), clip = "on") +
  labs(
    fill = "Reported\ncases",
    x = "Date of reported case",
    y = "New reported cases"
  ) +
  xlim(lubridate::ymd("2020-02-29"), .last_day)

# daily_diff_plot

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

# Timeline --------------------------------------------------------------------

d <- readr::read_csv(here::here("data-raw/timeline.csv"), comment = "#")
d <- d[-1, ]

d$event <- gsub("outside Canada", "outside\nCanada", d$event)
d$event <- gsub("\\/cafe", "", d$event)
# d$type <- c("A", rep("B", length(d$event) - 1))
d$type <- c(rep("B", length(d$event)))
g_timeline <- ggplot(d, aes(date, 0, label = event)) +
  theme_void() +
  ylim(0, 1) +
  annotate("line",
    x = seq(min(d$date) - 1.15,
      max(d$date) + 0.9, by = "1 day"),
    y = 0, col = "grey70"
  ) +
  # coord_flip(clip = "off") +
  coord_cartesian(clip = "off", expand = FALSE, ylim = c(0, 1)) +
  geom_point(col = "grey30", size = 1.3) +
  geom_text(y = 0.04, angle = 90, hjust = 0, aes(colour = type), lineheight = .75, size = 3.3) +
  # scale_color_manual(values = c(RColorBrewer::brewer.pal(6, "Blues")[5], "grey10")) +
  scale_color_manual(values = c("grey20")) +
  guides(colour = FALSE) +
  # geom_text(y = -0.05, aes(label = substr(as.character(date), 6, 10)), angle = 30, hjust = 1, size = 3.3) +
  geom_text(y = -0.03, aes(label = format(date, "%b %d")), angle = 90, hjust = 1, size = 3.0, col = "grey30") +
  theme(plot.margin = margin(t = -10, r = -10, b = -20, l = 5, unit = "pt"))
  # xlim(lubridate::ymd("2020-02-29"), .last_day)

g_timeline
# ggsave("figs-ms/timeline.png", width = 6.45, height = 3.6, dpi = 400)
# ggsave("figs-ms/timeline.pdf", width = 6.45, height = 3.6)

g <- cowplot::plot_grid(g_timeline, plotdelay2, daily_diff_plot, hosp_plot,
  ncol = 2,
  labels = "AUTO", align = "hv", label_x = 0.18, label_y = 0.962
) +
  cowplot::draw_line(
    x = c(0.203, 0.132),
    y = c(0.483, 0.583),
    color = "grey70", size = 0.5, alpha = 0.6
  ) +
  cowplot::draw_line(
    x = c(0.284, 0.460),
    y = c(0.483, 0.584),
    color = "grey70", size = 0.5, alpha = 0.6
  )

# g
# g <- cowplot::plot_grid(daily_diff_plot, plotdelay2, hosp_plot, g_timeline,
#   ncol = 2,
#   labels = "AUTO", align = "hv", label_x = 0.18, label_y = 0.962
# )
# g
# cowplot::plot_grid(g, g_timeline, ncol = 2, rel_widths = c(2, 1)) +
#   theme(plot.margin = margin(2, 11, 2, 0))


ggsave("figs-ms/onset-hosp.png", width = 7.7, height = 5.1, dpi = 400)
ggsave("figs-ms/onset-hosp.pdf", width = 7.7, height = 5.1)
