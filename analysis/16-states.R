d <- readr::read_csv("https://covidtracking.com/api/v1/states/daily.csv")

library(ggplot2)
library(dplyr)

d$date <- lubridate::ymd(d$date)

g <- d %>% filter(date > lubridate::ymd("2020-03-01")) %>%
  ggplot(aes(date, positiveIncrease)) +
  geom_line() +
  facet_wrap(~state, scales = "free_y") +
  ggsidekick::theme_sleek() +
  theme(axis.text.x = element_text(angle = 90))
ggsave("figs-ms/states-positive-increase.png", width = 12, height = 8)

g <- d %>% filter(date > lubridate::ymd("2020-03-01")) %>%
  ggplot(aes(date, hospitalizedIncrease)) +
  geom_line() +
  facet_wrap(~state, scales = "free_y") +
  ggsidekick::theme_sleek() +
  theme(axis.text.x = element_text(angle = 90))
ggsave("figs-ms/states-hospitalized-increase.png", width = 12, height = 8)

g <- d %>% filter(date > lubridate::ymd("2020-03-01")) %>%
  ggplot(aes(date, totalTestResultsIncrease)) +
  geom_line() +
  facet_wrap(~state, scales = "free_y") +
  ggsidekick::theme_sleek() +
  theme(axis.text.x = element_text(angle = 90))
ggsave("figs-ms/states-tests-increase.png", width = 12, height = 8)
