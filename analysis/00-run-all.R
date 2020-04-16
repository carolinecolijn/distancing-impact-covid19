# Make all the figures for the paper:

library("here")

source(here("analysis/01-simulation-test.R"))

rm(list = ls()) # to avoid 'future' package exporting large objects
source(here("analysis/05-main-fig.R"))

rm(list = ls())
source(here("analysis/06-cycle-f2.R"))

rm(list = ls())
source(here("analysis/07-threshold.R"))

rm(list = ls())
source(here("analysis/08-f-projections.R"))

rm(list = ls())
source(here("analysis/09-delay.R"))

rm(list = ls())
source(here("analysis/09-rw.R"))

rm(list = ls())
source(here("analysis/10-sensitivity.R"))

if (Sys.info()[["user"]] == "seananderson") {
  # Data cannot be publicly released:
  source(here("analysis/11-onset-date.R"))
}
source(here("analysis/13-timeline.R"))

rm(list = ls())
source(here("analysis/14-sensitivity2.R"))

source(here("analysis/99-optimize.R"))
