source("data-model-prep.R")

h <- readr::read_csv("hospitalization-data.csv")
h <- mutate(h, Date = lubridate::dmy(Date))

d <- bind_rows(
  select(h, date = Date, Count = `Hosp Census`) %>% mutate(Type = "Hospital"),
  select(dat, date = Date, Count = `ICU Census`) %>% mutate(Type = "ICU")
)

ggplot(d, aes(date, Count, colour = Type)) +
  geom_point(aes(shape = Type)) +
  geom_line() +
  coord_cartesian(
    expand = FALSE, ylim = c(0, 160),
    xlim = range(d$date) + c(14, 1)
  ) +
  theme(
    axis.title.x.bottom = element_blank(),
    legend.position = c(0.15, 0.85),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 11)
  ) +
  scale_color_manual(values = c(.hist_blue, "grey45")) +
  scale_shape_manual(values = c(21, 19)) +
  labs(colour = "Census type", shape = "Census type")

ggsave("figs-ms/hospital-counts.png", width = 4.2, height = 2.8)
