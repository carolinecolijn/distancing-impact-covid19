# start by sourcing `02-seeiqr-fit.R`

library(ggplot2)
library(dplyr)
theme_set(theme_light())
source("make_quick_plots.R")

wd <- getwd()
setwd(here::here("selfIsolationModel", "stan"))
dir.create("figs", showWarnings = FALSE)

.today <- lubridate::today()
make_quick_plots(m2, id = paste0("-offset-NB2-", .today))
make_quick_plots(m, id = "-NB2")

setwd(wd)
