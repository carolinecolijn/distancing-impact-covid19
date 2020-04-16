library("rstan")
library("dplyr")
library("ggplot2")
library("future")
library("here")
rstan_options(auto_write = TRUE)
dir.create("data-generated", showWarnings = FALSE)
dir.create("figs-ms", showWarnings = FALSE)
dir.create("figs", showWarnings = FALSE)
options(mc.cores = parallel::detectCores() / 2)
# remotes::install_github("seananderson/ggsidekick")
theme_set(ggsidekick::theme_sleek())
dat <- here("data-raw/BC-Case-Counts_09.04.2020.csv") %>%
  readr::read_csv(col_types = readr::cols(
  Date = readr::col_character(),
  BC = readr::col_double(),
  `ICU Census` = readr::col_double(),
  new_icu_confirmed = readr::col_double()
))
names(dat)[names(dat) == "BC"] <- "Cases"
dat$Date <- lubridate::dmy(dat$Date)
dat$day <- seq_len(nrow(dat))
dat$daily_diffs <- c(
  dat$Cases[2] - dat$Cases[1],
  diff(dat$Cases)
)
.today <- max(dat$Date)
dat <- dplyr::filter(dat, Date >= "2020-03-01")
daily_diffs <- dat$daily_diffs
if (.today == "2020-04-08") {
  daily_diffs <- c(daily_diffs, 34, 40, 35) # April 9, 10, 11
  .today <- "2020-04-11"
}
tibble(
  date = seq(dat$Date[1], lubridate::ymd(.today), by = "1 day"),
  cases = daily_diffs
) %>%
  readr::write_csv(here("data-generated/daily-cases.csv"))
seeiqr_model <- rstan::stan_model(here("analysis/seeiqr.stan"))
source(here("analysis/fit_seeiqr.R"))
source(here("analysis/functions_sir.R"))
source(here("analysis/make_projection_plot.R"))
source(here("analysis/make_quick_plots.R"))

.hist_blue <- RColorBrewer::brewer.pal(6, "Blues")[5]
