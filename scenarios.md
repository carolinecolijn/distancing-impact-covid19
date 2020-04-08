-nb2-: Set up the way the R code was:

```
R0 = estimated; Lognormal(log(2.6), 0.2) prior
phi = NB2 dispersion parameter; estimated; 1/sqrt(phi) ~ Normal(0, 1) prior
f1 = 1.0
f2 = estimated with Beta prior mean = 0.4, SD = 0.15; starting decline on day 15 (from March 1) fully at f2 on day 22.
projected f2 = f2 posterior as estimated
sampled_fraction1 = 0.35,
sampled_fraction2 = 0.70,
sampled_fraction_day_change = 14,
pars = c(
  N = 4.4e6, D = 5, k1 = 1 / 5,
  k2 = 1, q = 0.05,
  r = 1, ur = 0.4, f1 = 1.0,
  start_decline = 15, # in days
  end_decline = 22 # in days
)
i0 = 8,
fsi = pars[["r"]] / (pars[["r"]] + pars[["ur"]]),
nsi = 1 - fsi
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
)
```
