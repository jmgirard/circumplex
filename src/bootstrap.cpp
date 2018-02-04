# include <RcppArmadillo.h>
# include <RcppArmadilloExtensions/sample.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

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
  int n = levels.size();
  int m = X.ncol();
  arma::mat out(n, m);
  for (int i(0); i < n; i++) {
    int level = levels(i);
    arma::mat sub = submat(X, T, level);
    arma::rowvec colmeans = col_means(sub);
    out.row(i) = colmeans;
  }
  return out;
}