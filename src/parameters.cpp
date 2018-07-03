# include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// Calculate the inner product for matrix multiplication
double inner(arma::vec x, arma::vec y) {
  arma::mat ip = x.t() * y;
  return ip(0);
} 

// Calculate the remainder after division
double remainder(double numerator, double denominator) {
  return(numerator - floor(numerator / denominator) * denominator);
}

//' Calculate structural summary parameters
//'
//' @param scores A numeric vector of scores on multiple circumplex scales: can
//'   be either mean scores or correlations.
//' @param angles A numeric vector containing an angular displacement for each
//'   circumplex scale provided in \code{scores} (in radians).
//' @return A numerical vector containing structural summary method parameters
//'   that describe \code{scores} given \code{angles}. The vector will contain
//'   the following values: elevation, x-axis value, y-axis value, amplitude,
//'   angular displacement (in radians), and model fit (R-squared).
//' @export
// [[Rcpp::export]]
NumericVector ssm_parameters(NumericVector scores, NumericVector angles) {
  double n = scores.size();
  double elev = mean(scores);
  double xval = (2 / n) * inner(scores, cos(angles));
  double yval = (2 / n) * inner(scores, sin(angles));
  double ampl = sqrt(pow(xval, 2) + pow(yval, 2));
  double disp = remainder(std::atan2(yval, xval), 2 * PI);
  double gfit = 1 - ((sum(pow(elev + ampl * cos(angles - disp) - scores, 2))) /
    (var(scores) * (n - 1)));
  NumericVector param = NumericVector::create(
    elev, xval, yval, ampl, disp, gfit
  );
  return param;
}

// Calculate the SSM parameters for each group where rows are groups
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

// Extract all rows of matrix X where T equals TestVal
arma::mat submat(NumericMatrix X, NumericVector T, int TestVal) {
  arma::mat Xmat(X.begin(), X.nrow(), X.ncol(), false);
  arma::colvec tIdx(T.begin(), T.size(), false); 
  arma::mat y = Xmat.rows(find(tIdx == TestVal));
  return y;
}

// Calculate the mean of each column in a matrix
arma::rowvec col_means(arma::mat x){
  arma::mat X = arma::mat(x.begin(), x.n_rows, x.n_cols, false); 
  return arma::mean(X, 0); 
}

// [[Rcpp::export]]
arma::mat group_scores(NumericMatrix X, NumericVector T) {
  NumericVector levels = unique(T);
  std::sort(levels.begin(), levels.end());
  int n = levels.size();
  int m = X.ncol();
  arma::mat out(n, m);
  for (int i(0); i < n; i++) {
    int level = levels(i);
    arma::mat sub = submat(X, T, level);
    arma::rowvec colmeans = col_means(sub);
    out.row(i) = colmeans;
  }
  return(out);
}

// Calculate the correlation of x and y vectors after pairwise deletion
double pairwise_r(arma::vec x, arma::vec y) {
  // Pairwise deletion
  int n = x.n_rows;
  arma::vec idx = arma::ones<arma::vec>(n);
  for (int i = 0; i < n; i++) {
    if (!arma::is_finite(x[i]) || !arma::is_finite(y[i])) idx[i] = 0;
  }
  arma::vec x2 = x.rows(find(idx == 1));
  arma::vec y2 = y.rows(find(idx == 1));
  // Calculation
  arma::mat r = arma::cor(x2, y2);
  return r(0, 0);
}

// [[Rcpp::export]]
arma::mat measure_scores(NumericMatrix scales, NumericMatrix measures) {
  int k = measures.ncol();
  int m = scales.ncol();
  arma::mat out(k, m);
  for (int i(0); i < k; i++) {
    for (int j(0); j < m; j++) {
      out(i, j) = pairwise_r(measures(_, i), scales(_, j));
    }
  }
  return out;
}