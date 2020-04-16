# Make all the figures for the paper:

setwd(here::here("analysis"))

source("01-simulation-test.R")
rm(list = ls()) # to avoid 'future' package exporting large objects

source("05-main-fig.R")

rm(list = ls())
source("06-cycle-f2.R")

rm(list = ls())
source("07-threshold.R")

rm(list = ls())
source("08-f-projections.R")

rm(list = ls())
source("09-delay.R")

rm(list = ls())
source("09-rw.R")

rm(list = ls())
source("10-sensitivity.R")

if (Sys.info()[["user"]] == "seananderson") {
  # Data cannot be publicly released:
  source("11-onset-date.R")
}
source("13-timeline.R")

rm(list = ls())
source("14-sensitivity2.R")

source("99-optimize.R")
