// This script implements functions in the class `fkrb` (defined in R6_fkrb.R) in C++

#include <cmath>

// Enable SuperLU to solve sparse systems
#define ARMA_USE_SUPERLU 1

//#include <Rcpp.h>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
arma::mat cpp_compute_ccps_dt( arma::vec delta, arma::vec dep_times, arma::rowvec supp_tau, double kappa ){
  
  // Get the number of points in the support and the number of products
  int length_supp = supp_tau.n_elem;
  int num_prods = dep_times.n_elem;
  
  // Compute utilities of each product
  // 1. Matrix with repetitions of delta along the columns
  arma::mat mat_delta = arma::repmat( delta, 1, length_supp );
  // 2. Matrix repeating the points of support/consumers' locations
  arma::mat mat_supp = arma::repmat( supp_tau, num_prods, 1 );
  // 3. Matrix repeating product locations
  arma::mat mat_locations = arma::repmat( dep_times, 1, length_supp );
  // 4. Matrix of u_{ji} -- product j, consumer location i
  arma::mat utilities = mat_delta - kappa * arma::pow( mat_supp - mat_locations, 2 );
  // 5. Add utilities of the outside option
  utilities.insert_rows( 0, 1 );
  // 6. Max utility for each type [i.e., look for max in each column]
  arma::rowvec vec_max_util = arma::max( utilities ); // See doc. By default this gives max of each column.
  // 7. Normalize utilities [note num_prods + 1 bc of the outside option]
  utilities = utilities - arma::repmat( vec_max_util, num_prods + 1, 1 );
  // 8. Exp utilities
  utilities = arma::exp( utilities );
  // 9. Type-specific inclusive values.
  arma::rowvec inc_values = sum( utilities ); // See doc. By default this gives the sum of each column.
  // 10. Choice probabilities for each type [note again num_prods + 1].
  arma::mat mat_ccps = utilities / repmat( inc_values, num_prods + 1, 1 );
  
  // Return (drop the prob of the outside option)
  mat_ccps.shed_row( 0 );
  return mat_ccps;
  
}

// [[Rcpp::export]]
arma::vec cpp_compute_mkt_shares_dt( arma::vec dist_tau, arma::vec delta, arma::vec dep_times, arma::rowvec supp_tau, double kappa ){
  
  // Compute CCPs
  arma::mat mat_ccps = cpp_compute_ccps_dt( delta, dep_times, supp_tau, kappa );
  
  // Return
  return mat_ccps * dist_tau;
  
}
