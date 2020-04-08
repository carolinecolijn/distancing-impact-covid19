# start by sourcing `02-seeiqr-fit.R`

library(ggplot2)
library(dplyr)
theme_set(theme_light())
source("make_quick_plots.R")

wd <- getwd()
setwd(here::here("selfIsolationModel", "stan"))
dir.create("figs", showWarnings = FALSE)

.today <- lubridate::today()
actual_dates <- dat$Date
actual_dates <- seq(actual_dates[1], actual_dates[length(actual_dates)] + 25, by = "1 day")

make_quick_plots(m[[1]], id = "-nb2", actual_dates = actual_dates)
make_quick_plots(m[[2]], id = "-nb2-f-1.0-forecast", actual_dates = actual_dates)

actual_dates <- seq(actual_dates[1], actual_dates[length(actual_dates)] + 90, by = "1 day")
make_quick_plots(m[[3]], id = "-NB2-0.3-sampled-long-projection", actual_dates = actual_dates)
# make_quick_plots(m[[4]], id = "-NB2-0.3-detection-0.2-f-projection", actual_dates = actual_dates)

setwd(wd)
