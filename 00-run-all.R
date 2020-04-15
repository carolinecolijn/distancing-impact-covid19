# Make all the figures for the paper:

rm(list = ls()) # so future package doesn't export any large objects

source("00-simulation-test.R")
rm(list = ls())

# source("01-seeiqr-sim.R")
# source("02-seeiqr-fit.R")
# source("03-seeiqr-plot.R")
# source("04-bc-cdc-projections.R")
source("05-fig1.R")
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
rm(list = ls())

source("11-onset-date.R")
rm(list = ls())

source("13-timeline.R")
rm(list = ls())

source("12-optimize.R")

