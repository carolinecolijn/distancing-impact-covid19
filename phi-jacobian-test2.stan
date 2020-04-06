parameters {
  real xi;
}
transformed parameters {
  real phi;
  phi = 1/sqrt(xi);
}
model {
  target += phi;
}
