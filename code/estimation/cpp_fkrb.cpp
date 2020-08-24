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

// [[Rcpp::export]]
arma::vec cpp_compute_pred_qty_mkt_month( arma::mat dt_mkt_month, arma::vec dist_tau, arma::rowvec supp_tau, double kappa ){
  
  // dt_mkt_month is a matrix with the following columns, ***in this order***:
    // day, prod_delta_idx, delta, time_of_day, market_size
  
  // 0. Define some objects needed below
  arma::vec days = dt_mkt_month.col( 0 );
  arma::uvec prod_delta_idx = conv_to< uvec >::from( dt_mkt_month.col( 1 ) );
  arma::vec delta = dt_mkt_month.col( 2 );
  arma::vec dep_times = dt_mkt_month.col( 3 );
  double market_size = dt_mkt_month( 0, 4 ); // I'm picking row 0 arbitrarily, bc I know that all entries of the market_size column are the same
  
  // 1. Define unique days in the data
  arma::vec unique_days = unique( days );
  // 2. Define unique prod_delta_idx
  arma::uvec unique_prod_delta_idx = unique( prod_delta_idx );
  // 3. Initialize predicted shares
  arma::vec predicted_shares( unique_prod_delta_idx.n_elem, arma::fill::zeros );
  // 4. Loop over days updating predicted shares
  // Before the loop, initialize objects used in it (and that don't change dimension)
  // Initialize idx_tmp to store the relevant prod_delta_idx
  arma::uword idx_tmp; // used in the inner loop
  
  for ( arma::uword ix = 0; ix < unique_days.n_elem; ix++ ){
    
    // Get the indices of the data corresponding to this day
    arma::uvec idx_in_dt = arma::find( days == unique_days( ix ) );
    
    // What prod_delta_idx appear in this day?
    arma::uvec prod_delta_idx_day = prod_delta_idx.elem( idx_in_dt );
    arma::uvec unique_prod_delta_idx_day = arma::unique( prod_delta_idx_day );
    
    // Compute daily market shares
    arma::vec mkt_shares_day = cpp_compute_mkt_shares_dt( dist_tau, delta.elem( idx_in_dt ), dep_times.elem( idx_in_dt ), supp_tau, kappa );
    
    // Loop over the prod_delta_idx that appear in the day, aggregating predicted_shares
    for ( arma::uword jx = 0; jx < unique_prod_delta_idx_day.n_elem; jx++ ){
      // What are the indices (of mkt_shares_day) that correspond to the ix-th prod_delta_idx?
      idx_tmp = unique_prod_delta_idx_day( jx ); // idx_tmp was initialized outside the loop
      arma::uvec prod_idx = arma::find( prod_delta_idx_day == idx_tmp );
      // Sum the corresponding market shares and add to predicted shares
      predicted_shares( idx_tmp - 1 ) += arma::sum( mkt_shares_day( prod_idx ) ); // - 1 for 0-based indexing
    }
    
  }
  
  // Return predicted quantities
  return market_size * predicted_shares;
  
}

// [[Rcpp::export]]
arma::vec cpp_update_delta_mkt_month( arma::mat dt_mkt_month, arma::vec dist_tau, arma::rowvec supp_tau, double kappa ){
  
  // What prod_delta_idx appear in the data?
  arma::uvec prod_delta_idx = conv_to< uvec >::from( dt_mkt_month.col( 1 ) );
  arma::uvec unique_prod_delta_idx = arma::unique( prod_delta_idx );
  
  // Get the vector of delta's
  arma::vec delta = dt_mkt_month.col( 2 );
  // >>> NOTE <<< 
    // I can't just take unique( delta ) because arma::unique automatically sorts the result.
    // I also can't do delta( arma::find_unique( delta ) ) because delta may be equal for diff prod_delta_idx
    // Hence the code below. Note that unique_deltas will have repeated values in the case mentioned above.
  //arma::vec unique_deltas = delta( arma::find_unique( prod_delta_idx ) );
  // Line of code just above fails if, e.g., prod_delta_idx = c( 1, 1, 3, 2, 1 ): delta's aren't properly sorted.
  // Lines below (complicated!) does deliver unique delta's, ordered according to prod_delta_idx
  delta = delta( arma::sort_index( prod_delta_idx ) );
  arma::vec unique_deltas = delta.elem( arma::find_unique( arma::sort( prod_delta_idx ) ) );
  
  // Get the vector of observed daily quantities
  arma::vec observed_daily_qtys = dt_mkt_month.col( 5 );
  
  // Compute observed qtys at the market-month-product level
  // Initialize a vector to store those qtys
  arma::vec observed_qtys( unique_prod_delta_idx.n_elem );
  
  // Loop over unique prod_delta_idx aggregating qtys
  for ( arma::uword ix = 0; ix < unique_prod_delta_idx.n_elem; ix++ ){
    
    // Find the indices of the ix-th prod_delta_idx
    arma::uvec idx_in_dt = arma::find( prod_delta_idx == unique_prod_delta_idx( ix ) );
    
    // Aggregate quantities
    observed_qtys( ix ) = arma::sum( observed_daily_qtys.elem( idx_in_dt ) );
    
  }
  
  // Compute predicted qtys
  arma::vec predicted_qtys = cpp_compute_pred_qty_mkt_month( dt_mkt_month, dist_tau, supp_tau, kappa );
  
  // Update delta and return
  return unique_deltas + arma::log( observed_qtys ) - arma::log( predicted_qtys );
  //arma::vec delta_new = unique_deltas + arma::log( observed_qtys ) - arma::log( predicted_qtys );
  //return Rcpp::List::create( //Named( "prod_delta_idx" ) = Rcpp::NumericVector( unique_prod_delta_idx.begin(), unique_prod_delta_idx.end() ) );//,
                             //Named( "delta" ) = Rcpp::NumericVector( unique_deltas.begin(), unique_deltas.end() ) );//,
                             //Named( "observed_qtys" ) = Rcpp::NumericVector( observed_qtys.begin(), observed_qtys.end() ),
                             //Named( "predicted_qtys" ) = Rcpp::NumericVector( predicted_qtys.begin(), predicted_qtys.end() ),
                             //Named( "delta_new" ) = Rcpp::NumericVector( delta_new.begin(), delta_new.end() )
  //);
}

// [[Rcpp::export]]
Rcpp::List cpp_contraction_mapping( arma::mat dt_mkt_month, arma::vec dist_tau, arma::rowvec supp_tau, double kappa, double tol, int iter_max ){
  
  // Get prod_delta_idx, used repeatedly below
  arma::uvec prod_delta_idx = conv_to< uvec >::from( dt_mkt_month.col( 1 ) );
  
  // Initial guess: vector of delta's corresponding to each prod_delta_idx in the data, ordered according to prod_delta_idx.
  // First get all the delta's in the data.
  arma::vec delta = dt_mkt_month.col( 2 );
  // Sort them according to prod_delta_idx
  delta = delta( arma::sort_index( prod_delta_idx ) );
  // Get values corresponding to each prod_delta_idx (sorted).
  arma::vec delta_current = delta.elem( arma::find_unique( arma::sort( prod_delta_idx ) ) );
  // See comment in `cpp_update_delta_mkt_month` about the line of code below.
  // arma::vec delta_current = delta( arma::find_unique( prod_delta_idx ) );
  
  // Loop
  double dist_delta = 1.0;
  int iter = 1;
  arma::vec delta_new( delta_current.n_elem );
  
  while ( ( dist_delta >= tol ) && ( iter <= iter_max ) ){
    
    // Update delta's
    delta_new = cpp_update_delta_mkt_month( dt_mkt_month, dist_tau, supp_tau, kappa );
    // Compute the distance between delta's
    dist_delta = max( abs( delta_current - delta_new ) );
    // Update delta_current
    delta_current = delta_new;
    // Rcpp::Rcout << "delta = " << delta_current << std::endl;
    // Change delta in dt_mkt_month
    dt_mkt_month.col( 2 ) = delta_current.elem( prod_delta_idx - 1 ); // This is required for the `cpp_update_delta_mkt_month` call above.
    // Update iteration counter
    iter++;
    
  }
  
  // Return a list with two vectors: prod_delta_idx and delta
    // >>> NOTE <<<
    // This function is being called inside data.table, which asks that everything inside j = list(...)
    // be an atomic vector or a list.
    // Armadillo objects are *always* returned as matrices. That's why I'm converting from Armadillo to Rcpp.
    // Two unsuccessful tries and a successful one.
  
  /*
  IntegerVector out_prod_delta_idx = wrap( arma::unique( prod_delta_idx ) );
  NumericVector out_delta = wrap( delta_current );
  return Rcpp::List::create( Named( "prod_delta_idx" ) = out_prod_delta_idx,
                             Named( "delta" ) = out_delta
                               );
  */
  
  /*
  return Rcpp::List::create( Named( "prod_delta_idx" ) = Rcpp::as<IntegerVector>( wrap( arma::unique( prod_delta_idx ) ) ),
                             Named( "delta" ) = Rcpp::as<NumericVector>( wrap( delta_current ) )
                               );
  */
  
  // I do *not* fully understand what's going on here. It's based on https://stackoverflow.com/questions/14253069/convert-rcpparmadillo-vector-to-rcpp-vector
   // [See the response by Artem Klevtsov]
  
  prod_delta_idx = arma::unique( prod_delta_idx );
  return Rcpp::List::create( Named( "prod_delta_idx" ) = Rcpp::NumericVector( prod_delta_idx.begin(), prod_delta_idx.end() ),
                             Named( "delta" ) = Rcpp::NumericVector( delta_current.begin(), delta_current.end() )
  );
  
} 