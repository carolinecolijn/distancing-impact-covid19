#' Fit the Stan SEEIQR model
#'
#' @param daily_cases A vector of daily new cases
#' @param daily_tests An optional vector of daily test numbers. Should include
#'   assumed tests for the forecast. I.e. `length(daily_cases) + forecast_days =
#'   length(daily_cases)`
#' @param Model from `rstan::stan_model(seeiqr_model)`.
#' @param obs_model Type of observation model
#' @param forecast_days Number of days into the future to forecast
#' @param time_increment Time increment for ODEs and Weibull delay-model
#'   integration
#' @param days_back Number of days to go back for Weibull delay-model
#'   integration
#' @param R0_prior Lognormal log mean and SD for R0 prior
#' @param phi_prior SD of `1/sqrt(phi) ~ Normal(0, SD)` prior, where NB2(mu,
#'   phi) and `Var(Y) = mu + mu^2 / phi`.
#'   <https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations>
#' @param f2_prior Beta mean and SD for `f2` parameter (fraction of infection
#'   force?)
#' @param seed MCMC seed
#' @param chains Number of MCMC chains
#' @param iter MCMC iterations per chain
#' @param sampled_fraction1 Fraction sampled before
#'   `sampled_fraction_day_change`
#' @param sampled_fraction2 Fraction sampled at and after
#'   `sampled_fraction_day_change`
#' @param sampled_fraction_day_change Date fraction sample changes
#' @param fixed_f_forecast Optional fixed `f` for forecast.
#' @param pars A named numeric vector of fixed parameter values
#' @param i0 A scaling factor FIXME
#' @param fsi FIXME
#' @param nsi FIXME
#' @param state_0 Initial state: a named numeric vector
#' @param save_state_predictions Include the state predictions? `y_hat`
#'   Will make the resulting model object much larger.

fit_seeiqr <- function(daily_cases,
                       daily_tests = NULL,
                       seeiqr_model,
                       obs_model = c("NB2", "Poisson"),
                       forecast_days = 60, # a bit faster if this is decreased
                       time_increment = 0.2,
                       days_back = 60,
                       R0_prior = c(log(2.6), 0.2),
                       phi_prior = 1,
                       f2_prior = c(0.4, 0.15),
                       seed = 4,
                       chains = if (parallel::detectCores() > 8) 8 else 4,
                       iter = if (chains == 8) 400 else 800,
                       sampled_fraction1 = 0.35,
                       sampled_fraction2 = 0.70,
                       sampled_fraction_day_change = 14,
                       fixed_f_forecast = NULL,
                       pars = c(
                         N = 4.4e6, D = 5, k1 = 1 / 5,
                         k2 = 1, q = 0.05,
                         r = 1, ur = 0.4, f1 = 1.0,
                         start_decline = 15, # in days
                         end_decline = 22 # in days
                       ),
                       i0 = 8,
                       fsi = pars[["r"]] / (pars[["r"]] + pars[["ur"]]),
                       nsi = 1 - fsi,
                       state_0 = c(
                         S = nsi * (pars[["N"]] - i0),
                         E1 = 0.4 * nsi * i0,
                         E2 = 0.1 * nsi * i0,
                         I = 0.5 * nsi * i0,
                         Q = 0,
                         R = 0,
                         Sd = fsi * (pars[["N"]] - i0),
                         E1d = 0.4 * fsi * i0,
                         E2d = 0.1 * fsi * i0,
                         Id = 0.5 * fsi * i0,
                         Qd = 0,
                         Rd = 0
                       ),
                        save_state_predictions = FALSE,
                        delayScale = 12.0529283,
                        delayShape = 1.9720199
  ) {
  obs_model <- match.arg(obs_model)
  obs_model <- if (obs_model == "NB2") 1L else 0L
  x_r <- pars

  if (!is.null(daily_tests)) {
    stopifnot(length(daily_cases) + forecast_days == length(daily_tests))
    if (min(daily_tests) == 0) {
      warning("Replacing 0 daily tests with 1.")
      daily_tests[daily_tests == 0] <- 1
    }
  }
  stopifnot(
    names(x_r) ==
      c("N", "D", "k1", "k2", "q", "r", "ur", "f1", "start_decline", "end_decline")
  )
  stopifnot(
    names(state_0) == c("S", "E1", "E2", "I", "Q", "R", "Sd", "E1d", "E2d", "Id", "Qd", "Rd")
  )

  days <- seq(1, length(daily_cases) + forecast_days)
  last_day_obs <- length(daily_cases)
  time <- seq(-30, max(days) + forecast_days, time_increment)
  x_r <- c(x_r, if (!is.null(fixed_f_forecast)) fixed_f_forecast else 0)
  names(x_r)[length(x_r)] <- "fixed_f_forecast"
  x_r <- c(x_r, c("last_day_obs" = last_day_obs))

  # find the equivalent time of each day (end):
  get_time_id <- function(day, time) max(which(time <= day))
  time_day_id <- vapply(days, get_time_id, numeric(1), time = time)

  get_time_day_id0 <- function(day, time, days_back) {
    # go back `days_back` or to beginning if that's negative time:
    check <- time < (day - days_back)
    if (sum(check) == 0L) {
      1L
    } else {
      max(which(check))
    }
  }
  # find the equivalent time of each day (start):
  time_day_id0 <- vapply(days, get_time_day_id0, numeric(1),
    time = time, days_back = days_back
  )

  # FIXME: simplify this!
  sampFrac <- ifelse(seq_along(time) < time_day_id[sampled_fraction_day_change],
    sampled_fraction1, sampled_fraction2
  )

  beta_sd <- f2_prior[2]
  beta_mean <- f2_prior[1]
  beta_shape1 <- get_beta_params(beta_mean, beta_sd)$alpha
  beta_shape2 <- get_beta_params(beta_mean, beta_sd)$beta

  stan_data <- list(
    T = length(time),
    days = days,
    daily_cases = daily_cases,
    offset = if (is.null(daily_tests)) rep(log(1), length(days)) else log(daily_tests),
    N = length(days),
    y0 = state_0,
    t0 = min(time) - 1,
    time = time,
    x_r = x_r,
    delayShape = delayShape,
    delayScale = delayScale,
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
  map_estimate <- optimizing(
    seeiqr_model,
    data = stan_data
  )
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
  pars_save <- c("R0", "f2", "phi", "lambda_d", "y_rep")
  if (save_state_predictions) pars_save <- c(pars_save, "y_hat")
  fit <- sampling(
    seeiqr_model,
    data = stan_data,
    iter = iter,
    chains = chains,
    init = function() initf(stan_data),
    seed = seed, # https://xkcd.com/221/
    pars = pars_save
  )
  post <- rstan::extract(fit)
  list(fit = fit, post = post, phi_prior = phi_prior, R0_prior = R0_prior, f2_prior = f2_prior, obs_model = obs_model, sampFrac = sampFrac, state_0 = state_0, daily_cases = daily_cases, daily_tests = daily_tests, days = days, time = time, last_day_obs = last_day_obs, pars = x_r, f2_prior_beta_shape1 = beta_shape1, f2_prior_beta_shape2 = beta_shape2, stan_data = stan_data)
}

get_beta_params <- function(mu, sd) {
  var <- sd^2
  alpha <- ((1 - mu) / var - 1 / mu) * mu^2
  beta <- alpha * (1 / mu - 1)
  list(alpha = alpha, beta = beta)
}
