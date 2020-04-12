// [[Rcpp::plugins(cpp14)]]
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::NumericVector get_lambd_cpp(
    Rcpp::NumericVector time,
    double k2,
    Rcpp::NumericVector E2,
    Rcpp::NumericVector E2d,
    double delayShape,
    double delayScale,
    int N,
    int T,
    Rcpp::NumericVector sampFrac,
    Rcpp::NumericVector time_day_id0,
    Rcpp::NumericVector time_day_id) {

  Rcpp::NumericVector ft(T);
  Rcpp::NumericVector lambd(N);
  double sum_ft_inner;
  double x;

  double dx = time[1] - time[0];
  for (int n = 0; n < N; n++) {
    for (int t = 0; t < T; t++) {
      ft[t] = 0.0; // initialize at 0 across the full 1:T
    }
    for (int t = time_day_id0[n]; t <= time_day_id[n]; t++) { // t is an increment here starting at 1
      x = time[time_day_id[n]] - time[t];
      ft[t] = sampFrac[n] * k2 * (E2[t] + E2d[t]) *
        R::dweibull(time[time_day_id[n]] - time[t], delayShape, delayScale, false);
        // (delayShape/delayScale * pow(x/delayScale,delayShape-1) * exp(-pow(x/delayScale,delayShape)));
    }
    sum_ft_inner = 0.0;
    for (int t = time_day_id0[n] + 1; t <= time_day_id[n] - 1; t++) {
      sum_ft_inner += ft[t];
    }
    lambd[n] = 0.5 * dx *
      (ft[time_day_id0[n]] + 2 * sum_ft_inner + ft[time_day_id[n]]);
  }
  return lambd;
}
