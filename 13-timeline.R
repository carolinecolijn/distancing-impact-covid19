d <- readr::read_csv("timeline.csv", comment = "#")

library(ggplot2)

# d$event <- gsub("non-essential travel", "non-essential\ntravel", d$event)
d$event <- gsub("non-essential", "\nnon-essential", d$event)
d$type <- c("A", rep("B", length(d$event) - 1))
g <- ggplot(d, aes(date, 1, label = event)) + geom_point() + theme_void() +
  ylim(0.9, 1.4) +
  annotate("line",
    x = seq(lubridate::ymd("2020-03-07"), lubridate::ymd("2020-03-21"), by= "1 day"),
    y = 1) +
  coord_cartesian(clip = 'off') +
  geom_text(y = 1.015, angle = 90, hjust = 0, aes(colour = type), lineheight = .8) +
  scale_color_manual(values = c(RColorBrewer::brewer.pal(6, "Blues")[5], "grey10")) +
  guides(colour = FALSE) +
  geom_text(y = 0.985, aes(label = date), angle = 30, hjust = 1) +
  theme(plot.margin = margin(t = -10, r = -10, b = 0, l = 10, unit = "pt"))

print(g)
ggsave("figs-ms/timeline.png", width =  6, height = 4)
