#include "gg_LL.h"


//' @title Gamma-Gamma: Log-Likelihood Function
//'
//' @description
//' Calculates the Log-Likelihood value for the Gamma-Gamma model.
//'
//' @param vLogparams a vector containing the log of the parameters p, q, gamma
//' @param vX frequency vector of length n counting the numbers of purchases
//' @param vM_x the observed average spending for every customer during the calibration time.
//' @template template_param_rcppvn
//'
//' @details
//' \code{vLogparams} is a vector with the parameters for the Gamma-Gamma model.
//' It has three parameters (p, q, gamma). The scale parameter for each transaction
//' is distributed across customers according to a gamma distribution with
//' parameters q (shape) and gamma (scale).
//'
//'@return
//' Returns the Log-Likelihood value for the Gamma-Gamma model.
//'
//' @template template_references_gg
//'
//'
// [[Rcpp::export]]
double gg_LL(const arma::vec& vLogparams,
             const arma::vec& vX,
             const arma::vec& vM_x,
             const arma::vec& vN)
{

  const double p = std::exp(vLogparams(0));
  const double q = std::exp(vLogparams(1));
  const double gamma = std::exp(vLogparams(2));

  // Calculate the likelood for all != 0 values
  const arma::uvec vNonZero = find((vX != 0.0) && (vM_x != 0.0));

  // arma::vec vLL(vX.n_elem);
  const arma::vec vLL = q * std::log(gamma)
    + ((p * vX(vNonZero) - 1) % arma::log(vM_x(vNonZero)))
    + ((p * vX(vNonZero)) % arma::log(vX(vNonZero)))
    - (p * vX(vNonZero) + q) % arma::log(gamma + vM_x(vNonZero) % vX(vNonZero))
    - clv::vec_lbeta(p * vX(vNonZero), q);

  return -1 * arma::sum(vLL % vN(vNonZero));
}


