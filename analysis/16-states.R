d <- readr::read_csv("https://covidtracking.com/api/v1/states/daily.csv")

library(ggplot2)
library(dplyr)
library(reshape2)

d$date <- lubridate::ymd(d$date)
states <- c("NY", "FL", "WA", "CA")
d <- dplyr::filter(d, state %in% states)

# -------DATA------------------#
g <- d %>% filter(date > lubridate::ymd("2020-03-01")) %>%
  ggplot(aes(date, positiveIncrease)) +
  geom_line() +
  facet_wrap(~state, scales = "free_y") +
  ggsidekick::theme_sleek() +
  theme(axis.text.x = element_text(angle = 90))
ggsave("../figs-ms/states-positive-increase.png", width = 12, height = 8)

g <- d %>% filter(date > lubridate::ymd("2020-03-01")) %>%
  ggplot(aes(date, hospitalizedIncrease)) +
  geom_line() +
  facet_wrap(~state, scales = "free_y") +
  ggsidekick::theme_sleek() +
  theme(axis.text.x = element_text(angle = 90))
ggsave("../figs-ms/states-hospitalized-increase.png", width = 12, height = 8)

g <- d %>% filter(date > lubridate::ymd("2020-03-01")) %>%
  ggplot(aes(date, totalTestResultsIncrease)) +
  geom_line() +
  facet_wrap(~state, scales = "free_y") +
  ggsidekick::theme_sleek() +
  theme(axis.text.x = element_text(angle = 90))
ggsave("../figs-ms/states-tests-increase.png", width = 12, height = 8)

# ------- TIMELINES---------------#
# formatting needs to be fixed
timeline <- readr::read_csv(here::here("data-raw/timeline_states.csv"), comment = "#")
timeline <- melt(timeline, id="event")

timeline$type <- c("A", rep("B", length(timeline$event) - 1))
g <- ggplot(timeline, aes(value, 1, label = event, group = variable)) +
  facet_wrap(~variable)+
  geom_point() +
  theme_void() +
  ylim(0.9, 1.2) +
  annotate("line",
    x = seq(lubridate::ymd("2020-03-12"),
      lubridate::ymd("2020-04-01"), by = "1 day"),
    y = 1
  ) +
  coord_cartesian(clip = "off") +
  geom_text(y = 1.015, angle = 90, hjust = 0, aes(colour = type), lineheight = .82) +
  #scale_color_manual(values = c(RColorBrewer::brewer.pal(6, "Blues")[5], "grey10")) +
  guides(colour = FALSE) +
  geom_text(y = 0.985, aes(label = value), angle = 30, hjust = 1) #+
  #theme(plot.margin = margin(t = -10, r = -10, b = -10, l = 5, unit = "pt"))


ggsave("../figs-ms/timeline_states.png", width = 6.45, height = 3.6, dpi = 400)
ggsave("../figs-ms/timeline_states.pdf", width = 6.45, height = 3.6)

############################################################################
# try to do some fits
############################################################################

library("covidseir")
options(mc.cores = parallel::detectCores() / 2) # Stan parallel processing

pop_size <- list("WA" = 7.615e6, "CA" = 39.51e6, "FL" = 21.48e6, "NY" = 19.45e6)

wa_dat <- filter(d, state == "WA")
wa_dat$day <- seq_len(nrow(wa_dat))

# do some tests with WA data 
# seems to be a bit better with higher i0 but ??
wa_fit <- fit_seir(wa_dat$positiveIncrease, chains = 4, iter = 250,
  samp_frac_fixed = rep(0.21, nrow(wa_dat)), 
  i0=300,
  pars = c(
    N = 7.615e6, D = 5, k1 = 1 / 5, k2 = 1, q = 0.05,
    r = 0.1, ur = 0.02, f0 = 1.0, start_decline = 11, end_decline = 22
  ))
print(wa_fit)

p <- forecast_seir(wa_fit, iter = 1:100, forecast_days = 5)
ggplot(p, aes(day, mu, group = .iteration)) +
    geom_line(alpha = 0.2) +
    geom_point(aes(x = day, y = positiveIncrease), data = wa_dat, inherit.aes = FALSE)
ggsave("../figs-ms/WA_testfit.png")

