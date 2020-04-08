functions{
  real[] seeiqr(real t,        // time
                real[] state,  // state
                real[] theta,  // parameters
                real[] x_r,    // data (real)
                int[]  x_i) {  // data (integer)

    real S     = state[1];
    real E1    = state[2];
    real E2    = state[3];
    real I     = state[4];
    real Q     = state[5];
    real R     = state[6];
    real Sd    = state[7];
    real E1d   = state[8];
    real E2d   = state[9];
    real Id    = state[10];
    real Qd    = state[11];
    real Rd    = state[12];

    real R0    = theta[1];
    real f2    = theta[2];

    real N     = x_r[1];
    real D     = x_r[2];
    real k1    = x_r[3];
    real k2    = x_r[4];
    real q     = x_r[5];
    real r     = x_r[6];
    real ur    = x_r[7];
    real f1    = x_r[8];
    real start_decline = x_r[9];
    real end_decline = x_r[10];
    real fixed_f_forecast = x_r[11];
    real last_obs_time = x_r[12];

    real dydt[12];

    real f;
    if (t < start_decline) {
      f = f1;
    } else if (t >= start_decline && t < end_decline) {
      f = f2 + (end_decline - t) * (f1 - f2) / (end_decline - start_decline);
    } else {
      f = f2;
    }
    if (t >= last_obs_time && fixed_f_forecast != 0) {
      f = f * fixed_f_forecast;
    }

    dydt[1]  = -(R0/(D+1/k2)) * (I + E2 + f*(Id+E2d)) * S/N - r*S + ur*Sd;
    dydt[2]  = (R0/(D+1/k2)) * (I + E2 + f*(Id+E2d)) * S/N - k1*E1 -r*E1 + ur*E1d;
    dydt[3]  = k1*E1 - k2*E2 - r*E2 + ur*E2d;
    dydt[4]  = k2*E2 - q*I - I/D - r*I + ur*Id;
    dydt[5]  = q*I - Q/D - r*Q + ur*Qd;
    dydt[6]  = I/D + Q/D - r*R + ur*Rd;

    dydt[7]  = -(f*R0/(D+1/k2)) * (I+E2 + f*(Id+E2d)) * Sd/N + r*S - ur*Sd;
    dydt[8]  = (f*R0/(D+1/k2)) * (I+E2 + f*(Id+E2d)) * Sd/N - k1*E1d +r*E1 - ur*E1d;
    dydt[9]  = k1*E1d - k2*E2d + r*E2 - ur*E2d;
    dydt[10] = k2*E2d - q*Id - Id/D + r*I - ur*Id;
    dydt[11] = q*Id - Qd/D + r*Q - ur*Qd;
    dydt[12] = Id/D + Qd/D + r*R - ur*Rd;

    return dydt;
  }
}
data {
  int<lower=0> T;     // number of time steps
  int<lower=0> N;     // number of days
  real y0[12];        // initial state
  real t0;            // first time step
  real time[T];       // time increments
  int days[N];        // day increments
  int last_day_obs;   // last day of observed data; days after this are projections
  int daily_diffs[last_day_obs]; // daily new case counts
  real offset[N];      // offset in case counts (log(tests))
  real x_r[12];       // data for ODEs (real numbers)
  real sampFrac[T];   // fraction of cases sampled per time step
  real delayScale;    // Weibull parameter for delay in becoming a case count
  real delayShape;    // Weibull parameter for delay in becoming a case count
  int time_day_id[N]; // last time increment associated with each day
  int time_day_id0[N];// first time increment for Weibull integration of case counts
  real R0_prior[2];   // lognormal log mean and SD for R0 prior
  real phi_prior[2];  // lognormal log mean and SD for phi prior [NB2(mu, phi)]
  real f2_prior[2];   // lognormal log mean and SD for f2 prior
  int<lower=0, upper=1> priors_only; // logical: include likelihood or just priors?
  int<lower=0, upper=1> est_phi;
  int<lower=0, upper=1> obs_model;
}
transformed data {
  int  x_i[0];      // fake; needed for ODE function
}
parameters {
 real R0;
 real<lower=0, upper=1> f2;
 real<lower=0> phi[est_phi]; // NB(2) (inverse) dispersion; est_phi turns it on or off
}
transformed parameters {
  real meanDelay = delayScale * tgamma(1 + 1 / delayShape);
  real dx = time[2] - time[1]; // time increment
  real ft[T];
  real lambda_d[N];
  real sum_ft_inner;
  real eta[N]; // expected value on link scale (log)

  real theta[2];
  real y_hat[T,12];

  theta[1] = R0;
  theta[2] = f2;

  y_hat = integrate_ode_rk45(seeiqr, y0, t0, time, theta, x_r, x_i);

  // FIXME: switch to Stan 1D integration function:
  for (t in 1:T) {
    ft[t] = 0; // initialize at 0, not technically needed
  }
  for (n in 1:N) {
    for (t in time_day_id0[n]:time_day_id[n]) {
        ft[t] = sampFrac[t] * x_r[4] * (y_hat[t,2] + y_hat[t,3]) *
                exp(weibull_lpdf(days[n] - time[t] | delayShape, delayScale));
    }
    sum_ft_inner = 0;
    for (t in (time_day_id0[n] + 1):(time_day_id[n] - 1)) {
      sum_ft_inner += ft[t];
    }
    lambda_d[n] = 0.5 * dx *
                 (time_day_id0[n] + 2 * sum_ft_inner + ft[time_day_id[n]]);
    eta[n] = log(lambda_d[n]) + offset[n]; // offset is likely `log(tests)`
  }

}
model {
  // priors:
  if (est_phi) {
    phi[1] ~ lognormal(phi_prior[1], phi_prior[2]);
  }
  R0 ~ lognormal(R0_prior[1], R0_prior[2]);
  f2 ~ beta(f2_prior[1], f2_prior[2]);

  // data likelihood:
  if (!priors_only) {
    for (n in 1:last_day_obs) {
      if (obs_model == 0) {
        daily_diffs[n] ~ poisson_log(eta[n]);
      } else {
        daily_diffs[n] ~ neg_binomial_2_log(eta[n], phi[1]);
      }
    }
  }
}
generated quantities{
  int y_rep[N]; // posterior predictive replicates
  // vector[last_day_obs] log_lik; // log_lik is for use with the loo package for LOOIC
  for (n in 1:N) {
    if (obs_model == 0) {
      y_rep[n] = poisson_log_rng(eta[n]);
    } else {
      y_rep[n] = neg_binomial_2_log_rng(eta[n], phi[1]);
    }
  }
  // for (n in 1:last_day_obs) {
  //   if (obs_model == 0) {
  //     log_lik[n] = poisson_log_lpmf(daily_diffs[n] | eta[n]);
  //   } else {
  //     log_lik[n] = neg_binomial_2_log_lpmf(daily_diffs[n] | eta[n], phi[1]);
  //   }
  // }
}
