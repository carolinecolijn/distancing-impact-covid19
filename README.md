## Estimating the impact of COVID-19 control measures using a Bayesian model of physical distancing

This repository contains code associated with a manuscript investigating the impact of COVID-19 control measures in British Columbia, Canada.

The main statistical model written in [Stan](https://mc-stan.org/) is available [here](analysis/seeiqr.stan) and the main R function that calls this model for a vector of daily case counts is available [here](analysis/fit_seeiqr.R). A function to make projection plots is available [here](analysis/make_projection_plot.R). This model may be released at a later date in a proper R package form.

Generally, any part of the analysis can be re-created by running one of the numbered R files starting with `01-...R` in the [`analysis`](analysis) folder. Alternatively, the file [`00-run-all.R`](analysis/make_projection_plot.R) can be sourced to run the entire analysis.

You will need the following packages installed:

```r
install.packages(c("tidyverse", "remotes", "rstan", "here", 
  "future", "deSolve", "furrr", "cowplot", "reshape2"))
remotes::install_github("https://github.com/seananderson/ggsidekick")
```
