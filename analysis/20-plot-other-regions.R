library(ggplot2)
library(dplyr)
library(here)
theme_set(ggsidekick::theme_sleek())
# library(future)
# plan(multisession, workers = 2)
ymd <- lubridate::ymd

set.seed(1029)
iter <- sample(1:1000, 500)
ny_fit <- readRDS(here("data-generated/new-york-fit.rds"))
ny_dat <- readRDS(here("data-generated/new-york-dat.rds"))
ny_prd <- covidseir::project_seir(ny_fit, forecast_days = 0, iter = iter)

fl_fit <- readRDS(here("data-generated/florida-fit.rds"))
fl_dat <- readRDS(here("data-generated/florida-dat.rds"))
fl_prd <- covidseir::project_seir(fl_fit, forecast_days = 0, iter = iter)

wa_fit <- readRDS(here("data-generated/washington-fit.rds"))
wa_dat <- readRDS(here("data-generated/washington-dat.rds"))
wa_prd <- covidseir::project_seir(wa_fit, forecast_days = 0, iter = iter)

nz_fit <- readRDS(here("data-generated/nz-fit.rds"))
nz_dat <- readRDS(here("data-generated/nz-dat.rds"))
nz_prd <- covidseir::project_seir(nz_fit, forecast_days = 0, iter = iter)

ca_fit <- readRDS(here("data-generated/california-fit.rds"))
ca_dat <- readRDS(here("data-generated/california-dat.rds"))
ca_prd <- covidseir::project_seir(ca_fit, forecast_days = 0, iter = 1:250)

save(ny_prd, fl_prd, wa_prd, nz_prd, ca_prd,
  file = here("data-generated/other-region-projections.rda"))
load(here("data-generated/other-region-projections.rda"))

purrr::walk(list(ny_fit, fl_fit, wa_fit, nz_fit, ca_fit), print)

source(here("analysis/get_thresh_covidseir.R"))
ny_thr <- get_thresh_covidseir(ny_fit, forecast_days = 30,
  fs = seq(0.2, 0.6, length.out = 5))
fl_thr <- get_thresh_covidseir(fl_fit, forecast_days = 30,
  fs = seq(0.2, 0.7, length.out = 5))
wa_thr <- get_thresh_covidseir(wa_fit, forecast_days = 30,
  fs = seq(0.2, 0.7, length.out = 5))
nz_thr <- get_thresh_covidseir(nz_fit, forecast_days = 30,
  fs = seq(0.2, 0.7, length.out = 5))
ca_thr <- get_thresh_covidseir(ca_fit, forecast_days = 30,
  fs = seq(0.2, 0.7, length.out = 5))
save(ny_thr, fl_thr, wa_thr, nz_thr, ca_thr,
  file = here("data-generated/other-region-thresholds.rda"))
load(here("data-generated/other-region-thresholds.rda"))

add_label <- function(letter, region, ymax) {
  list(cowplot::draw_label(letter, x = ymd("2020-03-07"),
    y = ymax * .88, hjust = 0, vjust = 0, fontface = "bold", size = 12),
    cowplot::draw_label(region, x = ymd("2020-03-12"),
      y = ymax * .88, hjust = 0, vjust = 0,fontface = "plain", size = 10))
}

source(here("analysis/plot_projection_w_inset.R"))
ymax <- 22000
ny_g <- plot_projection_w_inset(
  ny_prd, ny_dat, ny_fit, ylim = c(0, ymax)) +
  add_label("A", "New York", ymax)
# ny_g

ymax <- 2000
fl_g <- plot_projection_w_inset(
  fl_prd, fl_dat, fl_fit, ylim = c(0, ymax)) +
  add_label("B", "Florida", ymax) +
  theme(axis.title.y = element_blank())
# fl_g

ymax <- 700
wa_g <- plot_projection_w_inset(
  wa_prd, wa_dat, wa_fit, ylim = c(0, ymax)) +
  coord_cartesian(expand = FALSE, xlim = c(min(ny_dat$date), max(wa_dat$date)), ylim = c(0, ymax)) +
  add_label("C", "Washington", ymax)+
  theme(axis.title.y = element_blank())
# wa_g

ymax <- 60
nz_g <- plot_projection_w_inset(
  nz_prd, nz_dat, nz_fit, ylim = c(0, ymax)) +
  coord_cartesian(expand = FALSE, xlim = c(min(ny_dat$date), max(wa_dat$date)), ylim = c(0, ymax)) +
  add_label("E", "New Zealand", ymax) +
  theme(axis.title.y = element_blank())
# nz_g

ymax <- 3500
ca_g <- plot_projection_w_inset(
  ca_prd, ca_dat, ca_fit, ylim = c(0, ymax)) +
  add_label("D", "California", ymax)
# ca_g

g <- cowplot::plot_grid(
  ny_g,
  fl_g,
  wa_g,
  ca_g,
  nz_g,
  ncol = 3, align = "v")
# g
#
# .width <- 0.25
# .height = 0.26
# .x <- 0.19
# .x2 <- 0.59
# g1 <- g +
#   cowplot::draw_plot(f2_plot(ny_fit, 1-ny_thr),
#     x = .x, y = 0.9, width = .width, height = .height) +
#   cowplot::draw_plot(f2_plot(fl_fit, 1-fl_thr),
#     x = .x, y = 0.6, width = .width, height = .height) +
#   cowplot::draw_plot(f2_plot(wa_fit, 1-wa_thr),
#     x = .x, y = 0.4, width = .width, height = .height) +
#   cowplot::draw_plot(f2_plot(nz_fit, 1-nz_thr),
#     x = .x, y = 0.2, width = .width, height = .height)

ggsave(here("figs-ms/other-regions.pdf"), width = 7, height = 3.4)
# ggsave(here("figs-ms/other-regions.png"), width = 4, height = 7.5)


# wa_g <- plot_projection_w_inset(
#   wa_prd, wa_dat, wa_fit)
#
# cowplot::plot_grid(wa_g, f2_plot(wa_fit, 1-wa_thr), ncol = 1)
# ggsave(here("figs-ms/wa.pdf"), width = 4, height = 4.5)
#
#
