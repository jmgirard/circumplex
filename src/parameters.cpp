# include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

double inner(arma::vec x, arma::vec y) {
  arma::mat ip = x.t() * y;
  return ip(0);
} 

// [[Rcpp::export]]
NumericVector rads(NumericVector degrees) {
  return degrees * (PI / 180);
}

// [[Rcpp::export]]
double deg(double radians) {
  return radians * (180 / PI);
}

double remainder(double numerator, double denominator) {
  return(numerator - floor(numerator / denominator) * denominator);
}

//' Calculate structural summary parameters
//'
//' @param scores A numeric vector of scores on multiple circumplex scales: can
//'   be either mean scores or correlations.
//' @param angles A numeric vector containing an angular displacement for each
//'   circumplex scale provided in \code{scores} (in degrees).
//' @return A numerical vector containing structural summary method parameters
//'   that describe \code{scores} given \code{angles}. The vector will contain
//'   the following values: elevation, x-axis value, y-axis value, amplitude,
//'   angular displacement (in degrees), and model fit (R-squared).
//' @export
// [[Rcpp::export]]
NumericVector ssm_parameters(NumericVector scores, NumericVector angles) {
  double n = scores.size();
  double elev = mean(scores);
  double xval = (2 / n) * inner(scores, cos(rads(angles)));
  double yval = (2 / n) * inner(scores, sin(rads(angles)));
  double ampl = sqrt(pow(xval, 2) + pow(yval, 2));
  double disp = remainder(deg(std::atan2(yval, xval)), 360);
  double gfit = 1 - ((sum(pow(elev + ampl * cos(rads(angles - disp))
    - scores, 2))) / (var(scores) * (n - 1)));
  NumericVector param = NumericVector::create(
    _["e"] = elev,
    _["x"] = xval,
    _["y"] = yval,
    _["a"] = ampl,
    _["d"] = disp,
    _["fit"] = gfit
  );
  return param;
}

// [[Rcpp::export]]
std::vector<double> group_parameters(NumericMatrix scores, NumericVector angles) {
  double n = scores.nrow();
  std::vector<double> out;
  out.reserve(6 * n);
  for (int i(0); i < n; i++) {
    NumericVector iscores = scores(i, _);
    NumericVector iparams = ssm_parameters(iscores, angles);
    out.insert(out.end(), iparams.begin(), iparams.end());
  }
  return out;
}