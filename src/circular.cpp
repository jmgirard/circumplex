#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double angle_mean(NumericVector x) {
  double n = x.size();
  double S = 0;
  double C = 0;
  double out;
  // Decompose angles into rectangular coordinates
  for(int i(0); i < n; i++) {
    S += sin(x[i]);
    C += cos(x[i]);
  }
  double R = (sqrt(pow(S, 2) + pow(C, 2)) / n);
  // A vector length of zero has no angular mean
  if (R > DOUBLE_EPS) {
    out = std::atan2(S, C);
  } else {
    out = NA_REAL;
  }
  return out;
}

// [[Rcpp::export]]
double angle_dev(NumericVector theta, double xv) {
  double n = theta.size();
  double values = 0;
  for(int j(0); j < n; j++) {
    values += fabs(M_PI - fabs(theta[j] - xv));
  }
  values = values / n;
  values = M_PI - values;
  return values;
}

// [[Rcpp::export]]
double angle_median(NumericVector x) {
  x = x[!is_na(x)];
  double n = x.size();
  double dev_val;
  double minimum = M_PI;
  NumericVector candidates(1);
  // Find candidates for the median (with the minimum average deviation)
  for(int i(0); i < n; i++) {
    dev_val = angle_dev(x, x[i]);
    if(((dev_val - minimum) / n) < -DOUBLE_EPS) {
      minimum = dev_val;
      candidates[0] = x[i];
    } else if (fabs(dev_val - minimum) <= 1e-8) {
      candidates.push_back(x[i]);
    }
  }
  // The median is the angular mean of these candidates
  return angle_mean(candidates);
}

// Rescale to between -pi and pi radians (or -180 and 180 degrees)
// [[Rcpp::export]]
NumericVector compare_pi(NumericVector x) {
  int n = x.size();
  NumericVector y = clone(x);
  for (int i(0); i < n; i++) {
    // If less than -PI, add 2 * PI
    y[i] = (y[i] < -M_PI) ? (y[i] + (2 * M_PI)) : (y[i]);
    // If greater than PI, subtract 2 * PI
    y[i] = (y[i] > M_PI) ? (y[i] - (2 * M_PI)) : (y[i]);
    // If between -PI and PI, leave as is
  }
  return y;
}
