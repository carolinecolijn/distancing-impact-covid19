<!-- README.md is generated from README.Rmd. Please edit that file -->

### Estimating the impact of COVID-19 control measures using a Bayesian model of physical distancing

This repository contains code associated with a manuscript investigating
the impact of COVID-19 control measures in British Columbia, Canada.

The main statistical model written in [Stan](https://mc-stan.org/) is
available [here](analysis/seeiqr.stan) and the main R function that
calls this model for a vector of daily case counts is available
[here](analysis/fit_seeiqr.R). A function to make projection plots is
available [here](analysis/make_projection_plot.R). This model may be
released at a later date in a proper R package form.

Generally, any part of the analysis can be re-created by running one of
the numbered R files starting with `01-...R` in the
[`analysis`](analysis) folder. Alternatively, the file
[`00-run-all.R`](analysis/00-run-all.R) can be sourced to run the entire
analysis.

You will need the following packages installed:

``` r
install.packages(c("tidyverse", "remotes", "rstan", "here", 
  "future", "deSolve", "furrr", "cowplot", "reshape2"))
remotes::install_github("seananderson/ggsidekick")
```

An example of how to run the model:

``` r
library("rstan")
#> Loading required package: StanHeaders
#> Loading required package: ggplot2
#> rstan (Version 2.19.3, GitRev: 2e1f913d3ca3)
#> For execution on a local, multicore CPU with excess RAM we recommend calling
#> options(mc.cores = parallel::detectCores()).
#> To avoid recompilation of unchanged Stan programs, we recommend calling
#> rstan_options(auto_write = TRUE)
library("dplyr")
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library("ggplot2")
rstan_options(auto_write = TRUE) # Cache the compiled model
options(mc.cores = parallel::detectCores() / 2) # Parallel processing
d <- readr::read_csv("data-generated/daily-cases.csv")
#> Parsed with column specification:
#> cols(
#>   date = col_date(format = ""),
#>   cases = col_double()
#> )
d
#> # A tibble: 42 x 2
#>    date       cases
#>    <date>     <dbl>
#>  1 2020-03-01     0
#>  2 2020-03-02     0
#>  3 2020-03-03     1
#>  4 2020-03-04     3
#>  5 2020-03-05     1
#>  6 2020-03-06     8
#>  7 2020-03-07     0
#>  8 2020-03-08     6
#>  9 2020-03-09     5
#> 10 2020-03-10     0
#> # … with 32 more rows
```

``` r
seeiqr_model <- rstan::stan_model("analysis/seeiqr.stan")
source("analysis/fit_seeiqr.R")
source("analysis/make_projection_plot.R")
# Using fewer iterations for a quick example:
fit <- fit_seeiqr(d$cases, seeiqr_model = seeiqr_model,
  iter = 300, chains = 4)
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> http://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> http://mc-stan.org/misc/warnings.html#tail-ess
```

``` r
print(fit$fit, pars = c("R0", "f2", "phi"))
#> Inference for Stan model: seeiqr.
#> 4 chains, each with iter=300; warmup=150; thin=1; 
#> post-warmup draws per chain=150, total post-warmup draws=600.
#> 
#>        mean se_mean   sd 2.5%  25%  50%  75% 97.5% n_eff Rhat
#> R0     2.95    0.00 0.04 2.87 2.93 2.95 2.98  3.02   388 1.01
#> f2     0.22    0.00 0.07 0.10 0.17 0.22 0.26  0.35   332 1.00
#> phi[1] 7.22    0.15 2.56 3.40 5.51 6.81 8.39 14.06   301 1.01
#> 
#> Samples were drawn using NUTS(diag_e) at Thu Apr 16 20:14:33 2020.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
```

``` r
make_projection_plot(list(fit)) + theme_light()
```

![](README-figs/proj-plot-1.png)
