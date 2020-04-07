#' Fit the Stan SEEIQR model
#'
#' @param daily_diffs A vector of daily new cases
#' @param obs_model Type of observation model
#' @param forecast_days Number of days into the future to forecast
#' @param time_increment Time increment for Weibull delay-model integration
#' @param days_back Number of days to go back for Weibull delay-model integration
#' @param R0_prior Lognormal log mean and SD for R0 prior
#' @param phi_prior Lognormal log mean and SD for NB2 dispersion prior
#' @param f2_prior Beta mean and SD for `f2` parameter (fraction of infection force?)
#' @param iter MCMC iterations per chain
#' @param seed MCMC seed
#' @param chains Number of MCMC chains
#' @param sampled_fraction1 Fraction sampled before `sampled_fraction_day_change`
#' @param sampled_fraction2 Fraction sampled at and after `sampled_fraction_day_change`
#' @param sampled_fraction_day_change Date fraction sample changes
#' @param pars A named numeric vector of fixed parameter values
#' @param i0 A scaling factor FIXME
#' @param fsi FIXME
#' @param nsi FIXME
#' @param state_0 Initial state: a named numeric vector

fit_seeiqr <- function(daily_diffs,
                       obs_model = c("NB2", "Poisson"),
                       forecast_days = 25,
                       time_increment = 0.2, # for Weibull integration
                       days_back = 50, # for Weibull integration
                       R0_prior = c(log(2.6), 0.2), # Lognormal log mean and SD
                       phi_prior = c(log(1), 0.5), # Lognormal log mean and SD
                       f2_prior = c(0.4, 0.1), # Beta mean and SD,
                       iter = 500, # MCMC iterations perching
                       seed = 42, # MCMC seed
                       chains = 4, # Number of MCMC chains
                       sampled_fraction1 = 0.35,
                       sampled_fraction2 = 0.70,
                       sampled_fraction_day_change = 14,
                       pars = c(
                         N = 4.4e6, D = 5, k1 = 1 / 5,
                         k2 = 1, q = 0.05,
                         r = 1, ur = 0.4, f1 = 1.0,
                         start_decline = 12, end_decline = 22
                       ),
                       i0 = 8,
                       fsi = x_r[["r"]] / (x_r[["r"]] + x_r[["ur"]]),
                       nsi = 1 - fsi,
                       state_0 = c(
                         S = nsi * (x_r[["N"]] - i0),
                         E1 = 0.4 * nsi * i0,
                         E2 = 0.1 * nsi * i0,
                         I = 0.5 * nsi * i0,
                         Q = 0,
                         R = 0,
                         Sd = fsi * (x_r[["N"]] - i0),
                         E1d = 0.4 * fsi * i0,
                         E2d = 0.1 * fsi * i0,
                         Id = 0.5 * fsi * i0,
                         Qd = 0,
                         Rd = 0
                       )) {
  obs_model <- match.arg(obs_model)
  obs_model <- if (obs_model == "NB2") 1L else 0L
  x_r <- pars

  stopifnot(
    names(x_r) ==
      c("N", "D", "k1", "k2", "q", "r", "ur", "f1", "start_decline", "end_decline")
  )
  stopifnot(
    names(state_0) == c("S", "E1", "E2", "I", "Q", "R", "Sd", "E1d", "E2d", "Id", "Qd", "Rd")
  )

  days <- seq(1, length(daily_diffs) + forecast_days)
  last_day_obs <- length(daily_diffs)
  time <- seq(-30, max(days) + forecast_days, time_increment)
  last_time_obs <- max(which(time < last_day_obs)) # FIXME: + 1?

  get_time_id <- function(day, time) max(which(time < day))
  time_day_id <- vapply(days, get_time_id, numeric(1), time = time)

  get_time_day_id0 <- function(day, time, days_back) {
    check <- time < (day - days_back)
    if (sum(check) == 0L) {
      1L
    } else {
      max(which(check))
    }
  }
  time_day_id0 <- vapply(days, get_time_day_id0, numeric(1),
    time = time, days_back = days_back
  )

  sampFrac <- ifelse(seq_along(time) < time_day_id[sampled_fraction_day_change], sampled_fraction1, sampled_fraction2)

  get_beta_params <- function(mu, sd) {
    var <- sd^2
    alpha <- ((1 - mu) / var - 1 / mu) * mu^2
    beta <- alpha * (1 / mu - 1)
    list(alpha = alpha, beta = beta)
  }

  beta_sd <- f2_prior[2]
  beta_mean <- f2_prior[1]
  beta_shape1 <- get_beta_params(beta_mean, beta_sd)$alpha
  beta_shape2 <- get_beta_params(beta_mean, beta_sd)$beta

  stan_data <- list(
    T = length(time),
    days = days,
    daily_diffs = daily_diffs,
    offset = rep(log(1), length(days)),
    N = length(days),
    y0 = state_0,
    t0 = min(time) - 1,
    time = time,
    x_r = x_r,
    delayShape = 1.9720199,
    delayScale = 12.0529283,
    sampFrac = sampFrac,
    time_day_id = time_day_id,
    time_day_id0 = time_day_id0,
    R0_prior = R0_prior,
    phi_prior = phi_prior,
    f2_prior = c(beta_shape1, beta_shape2),
    priors_only = 0L,
    last_day_obs = last_day_obs,
    obs_model = obs_model,
    est_phi = if (obs_model == 1L) 1L else 0L
  )

  seeiqr_model <- stan_model("seeiqr.stan")
  map_estimate <- optimizing(
    seeiqr_model,
    data = stan_data
  )
  map_estimate$par["R0"]
  map_estimate$par["f2"]
  map_estimate$par["phi[1]"]

  initf <- function(stan_data) {
    R0 <- rlnorm(1, log(map_estimate$par[["R0"]]), 0.3)
    f2 <- rbeta(
      1,
      get_beta_params(map_estimate$par[["f2"]], 0.1)$alpha,
      get_beta_params(map_estimate$par[["f2"]], 0.1)$beta
    )
    init <- list(R0 = R0, f2 = f2)
    if (stan_data$est_phi) {
      init <- c(init, list(
        phi =
          array(rlnorm(1, log(map_estimate$par[["phi[1]"]]), 0.1))
      ))
    }
    init
  }
  fit <- sampling(
    seeiqr_model,
    data = stan_data,
    iter = iter,
    chains = chains,
    init = function() initf(stan_data),
    seed = seed, # https://xkcd.com/221/
    pars = c("R0", "f2", "phi", "lambda_d", "y_hat", "y_rep")
  )
  # saveRDS(fit, file = "sir-fit.rds")
  # print(fit, pars = c("R0", "f2", "phi"))
  post <- rstan::extract(fit)
  list(fit = fit, post = post)
}
