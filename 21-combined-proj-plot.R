# Start with sourcing `06-cycle-f2.R` and `08-f-projections.R`
# if not run already to create the data objects read in here.

source(here::here("analysis/data-model-prep.R"))

# Cycling: ------------------------------------------------------------

m <- readRDS("data-generated/main-fit-2000.rds")

.last_day <- m$last_day_obs
.last_day

pred_4x4 <- readRDS("data-generated/pred_4x4.rds")

# re-sample obs. model for smoother plots:
pred_4x4 <- purrr::map_dfr(1:10, function(i) {
  pred_4x4$y_rep <- MASS::rnegbin(length(pred_4x4$y_rep),
    pred_4x4$lambda_d,
    theta = pred_4x4$phi
  )
  pred_4x4
})

pred_3x3 <- readRDS("data-generated/pred_3x3.rds")

# re-sample obs. model for smoother plots:
pred_3x3 <- purrr::map_dfr(1:10, function(i) {
  pred_3x3$y_rep <- MASS::rnegbin(length(pred_3x3$y_rep),
    pred_3x3$lambda_d,
    theta = pred_3x3$phi
  )
  pred_3x3
})

x <- prep_dat(pred_4x4)
.max <- max(x$y_rep$upr) * 1.04
cycling4 <- make_projection_plot(
  models = list(m), mu_dat = x$mu,
  y_rep_dat = x$y_rep, ylim = c(0, .max), points_size = 1.25
) +
  theme(plot.margin = margin(11 / 2, 11, 11 / 2, 11 / 2))
for (i in seq(1, 4, 2)) {
  .inc <- 7 * 4
  g_last_day <- dat$Date[1] + .last_day
  cycling4 <- cycling4 + annotate("rect",
    xmin = g_last_day + (i - 1) * .inc - 1,
    xmax = g_last_day + i * .inc - 1,
    ymin = 0, ymax = Inf, fill = "#00000012"
  )
}

x <- prep_dat(pred_3x3)
cycling3 <- make_projection_plot(
  models = list(m), mu_dat = x$mu,
  y_rep_dat = x$y_rep, ylim = c(0, .max), points_size = 1.25
) +
  theme(plot.margin = margin(11 / 2, 11, 11 / 2, 11 / 2))
for (i in seq(1, 6, 2)) {
  .inc <- 7 * 3
  g_last_day <- dat$Date[1] + .last_day
  cycling3 <- cycling3 + annotate("rect",
    xmin = g_last_day + (i - 1) * .inc - 1,
    xmax = g_last_day + i * .inc - 1,
    ymin = 0, ymax = Inf, fill = "#00000012"
  )
}

# Fixed f2 projections: -----------------------------------------------

m_fs <- readRDS("data-generated/f-proj-fits.rds")

proj0.6 <- make_projection_plot(m_fs[1], facet = TRUE, ylim = c(0, 140))

proj0.8 <- make_projection_plot(m_fs[2], facet = TRUE, ylim = c(0, 140))

# proj1.0 <- make_projection_plot(m_fs[3], facet = TRUE, ylim = c(0, 130))

.coord <- coord_cartesian(
  expand = FALSE, ylim = c(0, 140),
  xlim = c(lubridate::ymd("2020-03-01"), lubridate::ymd("2020-07-06"))
)
.coord <- list(
  .coord,
  theme(plot.margin = margin(t = 0, r = 0, b = -5, l = -10),
   axis.title.y.left = element_blank())
)

g <- cowplot::plot_grid(
  proj0.6+ .coord + theme(axis.text.x.bottom = element_blank()),
  proj0.8+ .coord+ theme(axis.text.x.bottom = element_blank(), axis.text.y = element_blank()),
  cycling3 + .coord,
  cycling4 + .coord + theme(axis.text.y = element_blank()),
  nrow = 2, labels = "AUTO", label_x = 0.06, label_y = 0.995, align = "hv"
) + theme(plot.margin = margin(t = 5, r = 5, b = 12, l = 26)) +
  cowplot::draw_text("Reported cases", x = -0.05, y = 0.5, angle = 90, size = 12, col = "grey35")

ggsave(here::here("figs-ms/combined-proj-plot.png"), width = 5.7, height = 3.6, dpi = 400)
ggsave(here::here("figs-ms/combined-proj-plot.pdf"), width = 5.7, height = 3.6)

