# include <RcppArmadillo.h>
# include <limits>

using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

// Calculate the inner product for matrix multiplication
double inner(arma::vec x, arma::vec y) {
  arma::mat ip = x.t() * y;
  return ip(0);
}

// Calculate non-integer modulus
double modu(double x, double y) {
  double out = (x - std::floor(x / y) * y);
  return out;
}

// Calculate structural summary parameters (angles and displacement in radians)
// Degenerate profiles are numerically detected at a machine-noise tolerance:
// a flat profile (sd below tolerance) has undefined displacement and fit; a
// zero-amplitude profile with real variance (e.g., a pure higher harmonic)
// has undefined displacement but a fit of exactly 0. NA is returned for the
// undefined parameters; warnings are the R layer's responsibility.
// [[Rcpp::export]]
arma::vec ssm_parameters_cpp(arma::vec scores, arma::vec angles) {
  double n = scores.size();
  double elev = arma::mean(scores);
  double xval = (2 / n) * inner(scores, arma::cos(angles));
  double yval = (2 / n) * inner(scores, arma::sin(angles));
  double ampl = std::sqrt(std::pow(xval, 2) + std::pow(yval, 2));
  double disp;
  double gfit;
  double vars = arma::var(scores);
  double sd = std::sqrt(vars);
  // Scale-aware tolerance for float-cancellation noise (~13 orders of
  // magnitude below any real variation, so genuinely small amplitudes pass)
  double tol = 8 * std::numeric_limits<double>::epsilon() * n *
    arma::abs(scores).max();
  if (!(sd > tol)) {
    // Flat profile (or NaN scores): no cosine structure to summarize
    disp = NA_REAL;
    gfit = NA_REAL;
  } else if (ampl <= tol) {
    // Zero first-harmonic amplitude: the model reduces to the mean
    disp = NA_REAL;
    gfit = 0;
  } else {
    disp = modu(std::atan2(yval, xval), 2 * M_PI);
    gfit = 1 - ((arma::sum(arma::pow(elev + ampl *
      arma::cos(angles - disp) - scores, 2))) / (vars * (n - 1)));
  }
  arma::vec out = {elev, xval, yval, ampl, disp, gfit};
  return out;
}

// Calculate the SSM parameters as vector for each group where rows are groups
// [[Rcpp::export]]
arma::vec group_parameters(arma::mat scores, arma::vec angles) {
  double n = scores.n_rows;
  arma::vec out = arma::zeros<arma::vec>(n * 6);
  for (int i(0); i < n; i++) {
    out.subvec(i * 6, i * 6 + 5) = ssm_parameters_cpp(scores.row(i).t(), angles);
  }
  return out;
}

// Calculate the mean of each column in matrix x (ignoring missing values)
// [[Rcpp::export]]
arma::rowvec col_means(arma::mat x) {
  arma::uword p = x.n_cols;
  arma::rowvec out = arma::zeros<arma::rowvec>(p);
  for (arma::uword i = 0; i < p; i++) {
    arma::colvec y = x.col(i);
    y = y.elem(find_finite(y));
    // A column with no finite values has no mean; return NA rather than calling
    // arma::mean() on an empty vector (which throws). This mirrors pairwise_r()'s
    // guard and lets the degenerate-replicate machinery absorb the resample.
    out(i) = y.n_elem == 0 ? NA_REAL : arma::mean(y);
  }
  return out;
}

// Calculate the mean of each column in scales by group
// [[Rcpp::export]]
arma::mat mean_scores(arma::mat cs, arma::vec grp, bool lwd) {
  arma::vec levels = arma::sort(arma::unique(grp));
  int ng = levels.size();
  int ps = cs.n_cols;
  arma::mat out = arma::zeros<arma::mat>(ng, ps);
  if (ng == 1) {
    if (lwd == true) {
      // Single group and LWD
      out = arma::mean(cs, 0);
    } else {
      // Single group and PWD
      out = col_means(cs);
    }
  } else{
    if (lwd == true) {
      // Multiple groups and LWD
      for (int g(0); g < ng; g++) {
        int level = levels(g);
        arma::mat gcs = cs.rows(arma::find(grp == level));
        out.row(g) = arma::mean(gcs, 0);
      }
    } else {
      // Multiple groups and PWD
      for (int g(0); g < ng; g++) {
        int level = levels(g);
        arma::mat gcs = cs.rows(arma::find(grp == level));
        out.row(g) = col_means(gcs);
      }
    }
  }
  return out;
}

// Calculate the correlation of x and y vectors after pairwise deletion
// [[Rcpp::export]]
double pairwise_r(const arma::colvec& x, const arma::colvec& y) {
  arma::uword n = x.n_elem;
  arma::uvec keep(n);
  arma::uword count = 0;

  for (arma::uword i = 0; i < n; i++) {
    if (std::isfinite(x(i)) && std::isfinite(y(i))) {
      keep(count) = i;
      count++;
    }
  }

  // A valid correlation requires at least two data points
  if (count < 2) {
    return NA_REAL;
  }

  // Slice out exactly the number of matching indices found
  arma::uvec valid_indices = keep.head(count);
  arma::mat r = arma::cor(x.elem(valid_indices), y.elem(valid_indices));
  return r(0, 0);
}

// Calculate the correlation of each measure with each scale by group
// [[Rcpp::export]]
arma::mat corr_scores(arma::mat cs, arma::mat mv, arma::vec grp, bool lwd) {
  arma::vec levels = arma::sort(arma::unique(grp));
  arma::uword ng = levels.size();
  arma::uword pm = mv.n_cols;
  arma::uword ps = cs.n_cols;
  arma::mat out = arma::zeros<arma::mat>(ng * pm, ps);
  if (ng == 1) {
    if (lwd == true) {
      // Single group and LWD
      out = arma::cor(mv, cs);
    } else {
      // Single group and PWD
      for (arma::uword m = 0; m < pm; m++) {
        arma::colvec x = mv.col(m);
        for (arma::uword s = 0; s < ps; s++) {
          arma::colvec y = cs.col(s);
          double rpw = pairwise_r(x, y);
          out(m, s) = rpw;
        }
      }
    }
  } else{
    if (lwd == true) {
      // Multiple groups and LWD
      for (arma::uword g = 0; g < ng; g++) {
        int level = levels(g);
        arma::mat gcs = cs.rows(arma::find(grp == level));
        arma::mat gmv = mv.rows(arma::find(grp == level));
        out.rows(g * pm, g * pm + pm - 1) = arma::cor(gmv, gcs);
      }
    } else {
      // Multiple groups and PWD
      for (arma::uword g = 0; g < ng; g++) {
        int level = levels(g);
        arma::mat gcs = cs.rows(arma::find(grp == level));
        arma::mat gmv = mv.rows(arma::find(grp == level));
        for (arma::uword m = 0; m < pm; m++) {
          arma::vec x = gmv.col(m);
          for (arma::uword s = 0; s < ps; s++) {
            arma::vec y = gcs.col(s);
            out(g * pm + m, s) = pairwise_r(x, y);
          }
        }
      }
    }
  }
  return out;
}
