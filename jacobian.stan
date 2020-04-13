data {
  int<lower=0> N;
  int y[N];
  int prior_only;
  real phi_prior;
  int prior_type;
}
parameters {
  real<lower=0> mu;
  real<lower=0> phi;
}
model {
  if (!prior_only)
  y ~ neg_binomial_2_log(log(mu), phi);

  if (prior_type == 0) {
    1/sqrt(phi) ~ normal(0, phi_prior);
    target += log(0.5) - 1.5 * log(phi); // Jacobian adjustment
  }

  if (prior_type == 1) {
    1/phi ~ normal(0, phi_prior);
    target += -2 * log(phi); // Jacobian adjustment
  }

  if (prior_type == 2)
  phi ~ normal(0, phi_prior);
}

