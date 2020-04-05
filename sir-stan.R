library(rstan)

# data {
#   int<lower=0> T;
#   real y0[12];
#   real t0;
#   real ts[T];
#   real theta[1];
#   real x_r[10];

sim <- stan(
  "sir.stan",
  data = list(
    T = ,
    y0 = ,
    ts = ,
    theta = ,
    x_r =
  )
)
