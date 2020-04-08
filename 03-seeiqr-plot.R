# start by sourcing `02-seeiqr-fit.R`

library(ggplot2)
library(dplyr)
theme_set(theme_light())
source("make_quick_plots.R")
setwd(here::here("selfIsolationModel", "stan"))
dir.create("figs", showWarnings = FALSE)
.today <- lubridate::today()

# in progress; not done!
make_quick_plots(m[[1]], id = paste0("-nb2-", .today))
make_quick_plots(m[[2]], id = paste0("-nb2-f-1.0-forecast-", .today))
make_quick_plots(m[[3]], id = paste0("-nb2-0.3-sampled-", .today))
make_quick_plots(m[[4]], id = paste0("-poisson-", .today))
make_quick_plots(m[[5]], id = paste0("-nb2-f-0.6-forecast-", .today))
make_quick_plots(m[[6]], id = paste0("-nb2-test-offset-", .today))
