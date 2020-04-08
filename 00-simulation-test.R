setwd(here::here("selfIsolationModel/stan"))
my_path <- paste0(here::here(), "/selfIsolationModel/")
source(paste0(my_path, "functions_sir.R"))  # libraries to load plus function definitions

pars = list(N = 4.4e6, #population of BC. Another potential issue as BC isn't homogeneously mixing; could fit to Vancouver instead
  D = 5,
  R0 = 2.65,
  k1 = 1/5,
  k2 = 1,
  q = 0.05,
  r = 1,
  ur = 0.4,
  f1 = 1.0,   # initial value of f
  f2 = 0.4,     # final value of f
  ratio = 2)
# AE thinks state is initial condition and i0 scales everything
fsi = with(pars,
  r/(r+ur))
nsi = 1 - fsi
i0 = 8
state_0 = c(S  = nsi * (pars$N - i0),
  E1 = 0.4 * nsi * i0,
  E2 = 0.1 * nsi * i0,
  I =  0.5 * nsi * i0,
  Q = 0,
  R = 0,
  Sd = fsi * (pars$N - i0),
  E1d = 0.4 * fsi * i0,
  E2d = 0.1 * fsi * i0,
  Id = 0.5 * fsi * i0,
  Qd = 0,
  Rd = 0)

times = seq(from=-30,   # should probably define values up front
  to=30,
  by=0.1)

sim_dat <- purrr::map(1:16, function(x) {
  example_simulation = as.data.frame(deSolve::ode(y = state_0,
    times = times,
    func = socdistmodel,
    parms = pars,
    sdtiming = sdtiming_gradual))

  # example_simulation %>% reshape2::melt(id.vars = "time") %>% ggplot(aes(time, value)) + facet_wrap(~variable, scales = "free_y") + geom_line()

  dat <- data.frame(
    Date = seq(lubridate::ymd("2020-03-01"),
      lubridate::ymd("2020-04-01"), by = "day"))
  dat$day <- seq_along(dat$Date)
  lambda_d <- sapply(seq(1, max(example_simulation$time)), function(x) {
    getlambd(example_simulation, pars = pars, data = dat, day = x)
  })

  # plot(seq(1, max(example_simulation$time)), lambda_d)

  sim_dat <- data.frame(day = seq(1, max(example_simulation$time)),
    lambda_d = lambda_d, obs = MASS::rnegbin(30, lambda_d, theta = 1.5))
  sim_dat
})

plot(sim_dat[[1]]$lambda_d)
plot(sim_dat[[1]]$obs)

source("fit_seeiqr.R")
library(future)
library(rstan)
library(dplyr)
library(ggplot2)
seeiqr_model <- stan_model("seeiqr.stan")
plan(multisession)
sim <- furrr::future_map(seq_along(sim_dat), function(x) {
  fit_seeiqr(
    daily_cases = sim_dat[[x]]$obs,
    seeiqr_model = seeiqr_model,
    forecast_days = 1,
    R0_prior = c(log(2.65), 0.2),
    f2_prior = c(0.4, 0.1), # first is fake for now
    iter = 500,
    chains = 1)
})
print(sim[[1]]$fit, pars = c("R0", "f2", "phi"))

check_sim <- function(.par) {
  out <- purrr::map_df(seq_along(sim), function(x) {
    data.frame(sim = x, parameter = sim[[x]]$post[[.par]])
  })
  .hline <- if (.par == "R0") {
    2.65
  } else if (.par == "f2") {
    0.4
  } else if (.par == "phi") {
    1.5
  }
  ggplot(out, aes_string("as.factor(sim)", "parameter")) +
    geom_boxplot() +
    geom_hline(yintercept = .hline) +
    ylab(.par)
}

g1 <- check_sim("R0")
g2 <- check_sim("phi")
g3 <- check_sim("f2")
cowplot::plot_grid(g1, g2, g3, ncol = 1)
ggsave("figs/R-sim-test.pdf", width = 6, height = 7)
ggsave("figs/R-sim-test.png", width = 6, height = 7)

# Try fitting to posterior predictive draws: -----------------------------

dat <- readr::read_csv(here::here("nCoVDailyData/CaseCounts/BC Case counts.csv"))
names(dat)[names(dat) == "BC"] <- "Cases"
dat$Date[71] <- "1/4/2020" # argh
dat$Date[72] <- "2/4/2020" # argh
dat$Date <- lubridate::dmy(dat$Date)
dat$day <- seq_len(nrow(dat))
dat$daily_diffs <- c(
  dat$Cases[2] - dat$Cases[1],
  diff(dat$Cases)
)
dat <- dplyr::filter(dat, Date >= "2020-03-01")
daily_diffs <- dat$daily_diffs
plot(daily_diffs)
fit <- fit_seeiqr(
  daily_diffs,
  chains = 1,
  iter = 700,
  forecast_days = 1,
  seeiqr_model = seeiqr_model)
print(fit$fit, pars = c("R0", "f2", "phi"))
fit$post$y_rep[1,]

ppd_sim <- furrr::future_map(1:16, function(x) {
  fit_seeiqr(
    daily_cases = fit$post$y_rep[x,],
    chains = 1,
    iter = 350,
    forecast_days = 1,
    seeiqr_model = seeiqr_model)
})

check_sim <- function(.par) {
  out <- purrr::map_df(seq_along(ppd_sim), function(x) {
    data.frame(sim = x, parameter = ppd_sim[[x]]$post[[.par]])
  })
  ggplot(out, aes_string("as.factor(sim)", "parameter")) + geom_boxplot() +
    geom_hline(yintercept = median(fit$post[[.par]])) +
    geom_hline(yintercept = quantile(fit$post[[.par]], 0.1)) +
    geom_hline(yintercept = quantile(fit$post[[.par]], 0.9)) +
    ylab(.par)
}

g1 <- check_sim("R0")
g2 <- check_sim("phi")
g3 <- check_sim("f2")
cowplot::plot_grid(g1, g2, g3, nrow = 3)
ggsave("figs/ppd-sim-test.pdf", width = 6, height = 7)
ggsave("figs/ppd-sim-test.png", width = 6, height = 7)
