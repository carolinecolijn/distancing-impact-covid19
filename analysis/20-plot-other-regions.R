library(ggplot2)
library(dplyr)
theme_set(ggsidekick::theme_sleek())

ny_fit <- readRDS(here::here("data-generated/new-york-fit.rds"))
ny_dat <- readRDS(here::here("data-generated/new-york-dat.rds"))
ny_prd <- covidseir::project_seir(ny_fit, forecast_days = 0)

fl_fit <- readRDS(here::here("data-generated/florida-fit.rds"))
fl_dat <- readRDS(here::here("data-generated/florida-dat.rds"))
fl_prd <- covidseir::project_seir(fl_fit, forecast_days = 0)

source(here::here("analysis/plot_projection_w_inset.R"))
ny_g <- plot_projection_w_inset(
  ny_prd, ny_dat, ny_fit,) +
  cowplot::draw_label("A", x = ymd("2020-03-07"), y = 18000, hjust = 0, vjust = 0, fontface = "bold", size = 12) +
  cowplot::draw_label("New York", x = ymd("2020-03-10"), y = 18000, hjust = 0, vjust = 0,fontface = "plain", size = 10)
fl_g <- plot_projection_w_inset(
  fl_prd, fl_dat, fl_fit) +
  cowplot::draw_label("B", x = ymd("2020-03-07"), y = 1750, hjust = 0, vjust = 0,fontface = "bold", size = 12) +
  cowplot::draw_label("Florida", x = ymd("2020-03-10"), y = 1750, hjust = 0,vjust = 0,fontface = "plain", size = 10)
# ny_g
# fl_g
g <- cowplot::plot_grid(ny_g, fl_g, ncol = 1, align = "v")

g1 <- g +
  cowplot::draw_plot(f2_plot(ny_fit), x = 0.19, y = 0.7, width = .25, height = .27) +
  cowplot::draw_plot(f2_plot(fl_fit), x = 0.19, y = 0.2, width = .25, height = .27)

ggsave(here::here("figs-ms/other-regions.pdf"), width = 4, height = 4.5)
ggsave(here::here("figs-ms/other-regions.png"), width = 4, height = 4.5)
