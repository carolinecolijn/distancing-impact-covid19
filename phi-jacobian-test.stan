parameters {
  real xi;
}
transformed parameters {
  real phi;
  phi = 1/sqrt(xi);
}
model {
  // log absolute determinant of the Jacobian of the transformation...
  // log absolute derivative of the transformation (1 / sqrt(x)):
  target += log(0.5 * phi^-0.5/sqrt(phi)^2);
}
