# This script defines a class used in the implementation of FKRB

library( R6 )

# Source the C++ version of `compute_market_shares_municipality`

fkrb <- R6Class( "fkrb", list(
  
  compute_ccps = function( prod_chars, supp_tau, kappa = 1, debug = FALSE ){
    
    # >>> Arguments <<<
      # prod_chars is a 2 x n_prods matrix. The j-th column has the chars of the j-th product.
        # The first row has the mean utility. The second row has the time of departure (scaled to [0,1])
      # supp_tau is a vector. It is the support of the discretized distribution of tau (ideal departure times).
      # kappa is the preference parameter in front of deviations from ideal locations.
    
    if ( debug ){
      browser()
    }
    
    # Compute the number of points in the support and the number of produts.
    length_supp <- length( supp_tau )
    num_prods <- ncol( prod_chars )
    
    # Compute utilities of each product
    # 1. Matrix with repetitions of delta along the columns
    mat_delta <- matrix( data = prod_chars[ 1, ], nrow = num_prods, ncol = length_supp )
    # 2. Matrix repeating the points of support/consumers' locations
    mat_supp <- matrix( data = supp_tau, nrow = num_prods, ncol = length_supp, byrow = TRUE )
    # 3. Matrix repeating product locations
    mat_locations <- matrix( data = prod_chars[ 2, ], nrow = num_prods, ncol = length_supp )
    
    # 4. Matrix of u_{ji} -- product j, consumer location i
    utilities <- mat_delta - kappa * ( mat_supp - mat_locations )^2
    # 5. Add utilities of the outside option
    utilities <- rbind( 0, utilities )
    # 6. Max utility for each type [apply max to each column]
    vec_max_util <- apply( X = utilities, MARGIN = 2, FUN = max )
    # 7. Normalize utilities [>>> Note num_prods + 1 bc of the outside option <<<]
    utilities <- utilities - matrix( data = vec_max_util, nrow = num_prods + 1, ncol = length_supp, byrow = TRUE )
    # 8. Exp utilities
    utilities <- exp( utilities )
    # 9. Choice probabilities for each type [>>> Note again num_prods + 1 <<<]
    mat_ccps <- utilities / matrix( data = colSums( utilities ), nrow = num_prods + 1, ncol = length_supp, byrow = TRUE )
    
    # Drop the outside option and return
    mat_ccps[ -1, ]
    
  },
  
  compute_ccps_dt = function( delta, dep_times, supp_tau, kappa = 1, debug = FALSE ){
    
    # >>> Comments <<<
      # This function does exactly the same as the function above, but it takes delta and departure times as arguments directly,
      # instead of the matrix `prod_chars` used in `compute_ccps`.
    
    if ( debug ){
      browser()
    }
    
    # Compute the number of points in the support and the number of produts.
    length_supp <- length( supp_tau )
    num_prods <- length( dep_times )
    
    # Compute utilities of each product
    # 1. Matrix with repetitions of delta along the columns
    mat_delta <- matrix( data = delta, nrow = num_prods, ncol = length_supp )
    # 2. Matrix repeating the points of support/consumers' locations
    mat_supp <- matrix( data = supp_tau, nrow = num_prods, ncol = length_supp, byrow = TRUE )
    # 3. Matrix repeating product locations
    mat_locations <- matrix( data = dep_times, nrow = num_prods, ncol = length_supp )
    
    # 4. Matrix of u_{ji} -- product j, consumer location i
    utilities <- mat_delta - kappa * ( mat_supp - mat_locations )^2
    # 5. Add utilities of the outside option
    utilities <- rbind( 0, utilities )
    # 6. Max utility for each type [apply max to each column]
    vec_max_util <- apply( X = utilities, MARGIN = 2, FUN = max )
    # 7. Normalize utilities [>>> Note num_prods + 1 bc of the outside option <<<]
    utilities <- utilities - matrix( data = vec_max_util, nrow = num_prods + 1, ncol = length_supp, byrow = TRUE )
    # 8. Exp utilities
    utilities <- exp( utilities )
    # 9. Choice probabilities for each type [>>> Note again num_prods + 1 <<<]
    mat_ccps <- utilities / matrix( data = colSums( utilities ), nrow = num_prods + 1, ncol = length_supp, byrow = TRUE )
    
    # Drop the outside option and return
    mat_ccps[ -1, ]
    
  },
  
  compute_mkt_shares = function( dist_tau, prod_chars, supp_tau, kappa ){
    
    # >>> Arguments <<<
      # prod_chars, supp_tau and kappa are as in `compute_ccps`
      # dist_tau is the (discretized) distribution of tau (consumers' ideal points)
    
    # Compute CCPs
    mat_ccps <- self$compute_ccps( prod_chars = prod_chars, supp_tau = supp_tau, kappa = kappa )
    
    # Integrate and return
    mat_ccps %*% dist_tau
    
  },
  
  compute_mkt_shares_dt = function( dist_tau, delta, dep_times, supp_tau, kappa ){
    
    # >>> Comments <<<
      # This function is just like `compute_mkt_shares` but takes delta and departure times directly as arguments,
      # instead of prod_chars
    
    # Compute CCPs
    mat_ccps <- self$compute_ccps_dt( delta = delta, dep_times = dep_times, supp_tau = supp_tau, kappa = kappa )
    
    # Integrate and return
    mat_ccps %*% dist_tau
    
  },
  
  compute_mkt_shares_coarse_delta = function( delta, prod_delta_idx, time_of_day, dist_tau, supp_tau, kappa = 1, debug = FALSE ){
    
    # >>> Arguments <<<
      # dist_tau, supp_tay and kappa are as in `compute_mkt_shares` and `compute_ccps`
      # delta is a vector of mean utilities at the market, firm, month, service level.
      # prod_delta_idx is a vector of indices to map from delta onto products.
      # time_of_day is the time of departure and comes from the data.
    
    if ( debug ){
      browser()
    }
    
    delta_prods <- delta[ prod_delta_idx ]
    prod_chars <- rbind( delta_prods, time_of_day )
    
    # Compute shares
    self$compute_mkt_shares( dist_tau = dist_tau, prod_chars = prod_chars, supp_tau = supp_tau, kappa = kappa )
    
  },
  
  contraction_mapping = function( dt_tmp, tol, iter_max, debug = FALSE ){
    
    if ( debug ){
      browser()
    }
    
    # Delta convergence flag
    conv_delta <- FALSE
    iter <- 1
    
    dt_tmp_copy <- copy( dt_tmp ) # I really didn't wanna do this =(
    
    while( !conv_delta ){
      
      # Compute shares, observed and predicted quantity and update delta
      dt_delta_new <- dt_tmp_copy[
        ,
        daily_mkt_shares := fkrb_inst$compute_mkt_shares_dt( dist_tau = dist_tau,
                                                             delta = delta,
                                                             dep_times = time_of_day,
                                                             supp_tau = supp_tau,
                                                             kappa = 1
                                                             ),
        by = .( data_viagem )
        ][
          ,
          .(
            delta = delta[[ 1 ]], # unique within the group by construction
            total_qty = sum( num_tix ),
            predicted_qty = sum( daily_mkt_shares ) * mkt_size
          ),
          by = .( prod_delta_idx )
          ][
            ,
            delta_new := delta + log( total_qty ) - log( predicted_qty )
            ]
      
      # Compute distance
      dist <- dt_delta_new[ , max( abs( delta - delta_new ) ) ]
      # Clean dt_delta_new
      dt_delta_new <- dt_delta_new[ , .( prod_delta_idx, delta = delta_new ) ]
      
      # Join the new delta onto dt_tmp_copy
      dt_tmp_copy[ , delta := NULL ]
      dt_tmp_copy <- dt_delta_new[ dt_tmp_copy, on = "prod_delta_idx" ]
      
      # Print comforting message
      # cat( "Distance is equal to ", dist, ".\n", sep = "" )
      
      # Update iteration counter
      iter <- iter + 1
      # Update convergence flag
      conv_delta <- ( dist < tol )
      #
      if ( iter > iter_max && !conv_delta ){
        stop( "The contraction mapping failed to converge." )
      }
      
    }
    
    # Return dt_tmp_copy with the updated delta
    dt_tmp_copy
    
  }
  
)

)