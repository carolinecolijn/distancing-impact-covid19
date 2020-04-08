# start by sourcing `02-seeiqr-fit.R`

library(ggplot2)
library(dplyr)
theme_set(theme_light())
source("make_quick_plots.R")

wd <- getwd()
setwd(here::here("selfIsolationModel", "stan"))
dir.create("figs", showWarnings = FALSE)
.today <- lubridate::today()

make_quick_plots(m[[1]], id = paste0("-nb2-", .today))
make_quick_plots(m[[2]], id = paste0("-nb2-f-1.0-forecast-", .today))
make_quick_plots(m[[3]], id = paste0("-nb2-0.3-sampled-", .today))
