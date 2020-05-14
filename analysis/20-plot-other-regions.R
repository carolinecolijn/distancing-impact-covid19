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

mean(ny_fit$post$f_s[,1] < ny_thr)
mean(fl_fit$post$f_s[,1] < fl_thr)
mean(wa_fit$post$f_s[,1] < wa_thr)
mean(ca_fit$post$f_s[,1] < ca_thr)
mean(nz_fit$post$f_s[,1] < nz_thr)

q <- list()
q[[1]] <- quantile(ny_fit$post$f_s[,1] / ny_thr, probs = c(0.05, 0.5, 0.95))
q[[2]] <- quantile(fl_fit$post$f_s[,1] / fl_thr, probs = c(0.05, 0.5, 0.95))
q[[3]] <- quantile(wa_fit$post$f_s[,1] / wa_thr, probs = c(0.05, 0.5, 0.95))
q[[4]] <- quantile(ca_fit$post$f_s[,1] / ca_thr, probs = c(0.05, 0.5, 0.95))
q[[5]] <- quantile(nz_fit$post$f_s[,1] / nz_thr, probs = c(0.05, 0.5, 0.95))

m_bc <- readRDS(here::here("data-generated/main-fit-2000.rds"))
threshold_bc <- readRDS(here::here("data-generated/BC-threshold.rds"))

q[[6]] <- quantile(m_bc$post$f2 / threshold_bc, probs = c(0.05, 0.5, 0.95))

names(q) <- c("NY", "FL", "WA", "CA", "NZ", "BC")

source(here::here("analysis/functions_sir.R"))
purrr::walk(seq_along(q), function(.x) {
  write_tex(sprintf("%.2f", round(q[[.x]][[1]], 2)),
    paste0(names(q)[.x], "lwrRatio"))
  write_tex(sprintf("%.2f", round(q[[.x]][[2]], 2)),
    paste0(names(q)[.x], "medRatio"))
  write_tex(sprintf("%.2f", round(q[[.x]][[3]], 2)),
    paste0(names(q)[.x], "uprRatio"))
})


cols <- RColorBrewer::brewer.pal(n = 6, "Dark2")[-(5)]
regions <- c("New Zealand",  "California" , "Florida" , "New York", "Washington")
names(cols) <- regions

add_label <- function(letter, region, ymax) {
  list(cowplot::draw_label(letter, x = ymd("2020-03-03"),
    y = ymax * .88, hjust = 0, vjust = 0, fontface = "bold", size = 12),
    cowplot::draw_label(region, x = ymd("2020-03-08") + 0.5,
      y = ymax * .88, hjust = 0, vjust = 0,fontface = "plain", size = 10))
}

source(here("analysis/plot_projection_w_inset.R"))
ymax <- 22000
ny_g <- plot_projection_w_inset(
  ny_prd, ny_dat, ny_fit, ylim = c(0, ymax), col = cols[["New York"]]) +
  add_label("A", "New York", ymax) +
  theme(axis.text.x.bottom = element_blank())
# ny_g

ymax <- 2000
fl_g <- plot_projection_w_inset(
  fl_prd, fl_dat, fl_fit, ylim = c(0, ymax), col = cols[["Florida"]]) +
  add_label("B", "Florida", ymax) +
  theme(axis.title.y = element_blank())+
  theme(axis.text.x.bottom = element_blank())
# fl_g

ymax <- 700
wa_g <- plot_projection_w_inset(
  wa_prd, wa_dat, wa_fit, ylim = c(0, ymax), col = cols[["Washington"]]) +
  # coord_cartesian(expand = FALSE,
  #   xlim = c(min(ny_dat$date), max(wa_dat$date)), ylim = c(0, ymax)) +
  add_label("C", "Washington", ymax)+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x.bottom = element_blank())
# wa_g

ymax <- 60
nz_g <- plot_projection_w_inset(
  nz_prd, nz_dat, nz_fit, ylim = c(0, ymax), col = cols[["New Zealand"]]) +
  coord_cartesian(expand = FALSE,
    xlim = c(ymd("2020-03-01"), max(wa_dat$date)), ylim = c(0, ymax)) +
  add_label("E", "New Zealand", ymax) +
  theme(axis.title.y = element_blank())
# nz_g

ymax <- 3500
ca_g <- plot_projection_w_inset(
  ca_prd, ca_dat, ca_fit, ylim = c(0, ymax), col = cols[["California"]]) +
  add_label("D", "California", ymax)
# ca_g

# Google data:
if (!file.exists(here("data-generated/google-mobility.rds"))) {
goog_dat <- readr::read_csv(
  "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=722f3143b586a83f")
saveRDS(goog_dat, file = here("data-generated/google-mobility.rds"))
} else {
  goog_dat <- readRDS(here("data-generated/google-mobility.rds"))
}
goog_dat$date <- lubridate::ymd(goog_dat$date)
goog_dat <- dplyr:::filter(goog_dat,
  country_region == "New Zealand" | sub_region_1 %in%
    c("Washington", "California", "New York", "Florida"))
goog_dat <- dplyr::filter(goog_dat, date >= lubridate::ymd("2020-03-01"))
goog_dat <- goog_dat %>%
  mutate(region = ifelse(country_region == "New Zealand",
    country_region, sub_region_1))

# get daily means
goog_dat <- goog_dat %>%
  group_by(date, region) %>%
  mutate(MeanTransit = mean(transit_stations_percent_change_from_baseline, na.rm = TRUE)) %>%
  mutate(MeanWorkplace = mean(workplaces_percent_change_from_baseline, na.rm = TRUE)) %>%
  mutate(MeanRec = mean(retail_and_recreation_percent_change_from_baseline, na.rm = TRUE))

# # Just transit, using mean
# ggplot(goog_dat, aes(date, MeanTransit, group = region, col = region)) +
#   geom_smooth(method = "gam", se = FALSE) +
#   geom_line(alpha = 0.5) +
#   labs(y = "% change from baseline", x = "") +
#   theme_minimal() +
#   scale_color_brewer(palette = "Dark2")

half_line <- 11/2
goog_panel <-
  ggplot(goog_dat, aes(date, transit_stations_percent_change_from_baseline,
  group = region, col = region)) +
  geom_smooth(method = "gam", se = FALSE) +
  geom_line(aes(date, MeanTransit), alpha = 0.5) +
  labs(y = "% from baseline", x = "") +
  ggsidekick::theme_sleek() +
  scale_colour_manual(values = cols) +
  theme(legend.position = "none") +
  theme(axis.title.x.bottom = element_blank(),
    plot.margin =
      margin(t = 5, r = 1.5, b = -8, l = -3),
    axis.title.y = element_text(angle = 90,
      margin = margin(r = 2), vjust = 1, size = 10)
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
  scale_y_continuous(breaks = c(0, -20, -40, -60, -80), labels = function(x) x/1) +
  coord_cartesian(expand = FALSE, xlim = c(ymd("2020-03-01"), max(ny_dat$date)), ylim = c(-95, 25)) +
  add_label("F", "Google transit station", 12)
# goog_panel

g <- cowplot::plot_grid(
  ny_g,
  fl_g,
  wa_g,
  ca_g,
  nz_g,
  goog_panel,
  ncol = 3, align = "hv", axis = "lbtr") +
  cowplot::draw_text("Reported cases", x = 0.011, y = 0.5, angle = 90, size = 10, col = "grey30") +
  theme(axis.title.x.bottom = element_blank(),
    plot.margin =
      margin(t = 2, r = 4, b = 15, l = 1))
# g

.width <- 0.115
.height = 0.24

xgap <- 0.333
.x1 <- 0.075
.x2 <- .x1 + xgap
.x3 <- .x2 + xgap

.y2 <- 0.695
.y1 <- 0.19

g1 <- g +
  cowplot::draw_plot(f2_plot(ny_fit, ny_thr, col = cols[["New York"]]),
    x = .x1, y = .y2, width = .width, height = .height) +
  cowplot::draw_plot(f2_plot(fl_fit, fl_thr, col = cols[["Florida"]]),
    x = .x2, y = .y2, width = .width, height = .height) +
  cowplot::draw_plot(f2_plot(wa_fit, wa_thr, col = cols[["Washington"]]),
    x = .x3 + 0.146, y = .y2 + 0.05, width = .width, height = .height) +
  cowplot::draw_plot(f2_plot(ca_fit, ca_thr, col = cols[["California"]]),
    x = .x1, y = .y1, width = .width, height = .height) +
  cowplot::draw_plot(f2_plot(nz_fit, nz_thr, col = cols[["New Zealand"]]),
    x = .x2 + 0.146, y = .y1 + 0.0, width = .width, height = .height)

ggsave(here("figs-ms/other-regions.pdf"), width = 7.2, height = 3.3)
ggsave(here("figs-ms/other-regions.png"), width = 7.2, height = 3.3, dpi = 400)
