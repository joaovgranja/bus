# This script defines a class used in the implementation of FKRB

library( R6 )
library( Rcpp )
library( RcppArmadillo )

# Source the C++ script with equivalent implementations
sourceCpp( file = "code/estimation/cpp_fkrb.cpp" )

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
  
  compute_ccps_dt = function( delta, dep_times, supp_tau, kappa = 1, return_ccp_out_option = FALSE, debug = FALSE ){
    
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
    
    # Return
    if ( return_ccp_out_option ){
      mat_ccps
    } else {
      mat_ccps[ -1, ]
    }
    
  },
  
  compute_mkt_shares = function( dist_tau, prod_chars, supp_tau, kappa, debug = FALSE ){
    
    # >>> Arguments <<<
      # prod_chars, supp_tau and kappa are as in `compute_ccps`
      # dist_tau is the (discretized) distribution of tau (consumers' ideal points)
    
    if ( debug ){
      browser()
    }
    
    # Compute CCPs
    mat_ccps <- self$compute_ccps( prod_chars = prod_chars, supp_tau = supp_tau, kappa = kappa )
    
    # Integrate and return
    mat_ccps %*% dist_tau
    
  },
  
  compute_mkt_shares_dt = function( dist_tau, delta, dep_times, supp_tau, kappa, debug = FALSE ){
    
    # >>> Comments <<<
      # This function is just like `compute_mkt_shares` but takes delta and departure times directly as arguments,
      # instead of prod_chars
    
    if ( debug ){
      browser()
    }
    
    # Compute CCPs
    mat_ccps <- self$compute_ccps_dt( delta = delta, dep_times = dep_times, supp_tau = supp_tau, kappa = kappa )
    
    # Integrate and return
    as.numeric( mat_ccps %*% dist_tau )
    
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
  
  compute_pred_qty_mkt_month = function( dt_mkt_month, dist_tau, supp_tau, kappa, debug = FALSE ){
    
    # This function is here just for development purposes.
    # It is written in the ***exact*** way that it will be implemented in C++/Armadillo.
    
    if ( debug ){
      browser()
    }
    
    # 0. Get some objects needed below
    delta <- dt_mkt_month[ , delta ]
    dep_times <- dt_mkt_month[ , time_of_day ]
    market_size <- dt_mkt_month[ , unique( market_size ) ]
    # 1. What days appear in the data?
    days <- dt_mkt_month[ , dia_viagem ]
    unique_days <- sort( unique( days ) )
    # 2. What prod_delta_idx appear in the data?
    prod_delta_idx <- dt_mkt_month[ , prod_delta_idx ]
    unique_prod_delta_idx <- sort( unique( prod_delta_idx  ) )
    # 3. Initialize predicted shares
    predicted_shares <- numeric( length = length( unique_prod_delta_idx ) )
    # 4. Loop over days updating predicted shares
    for ( ix in seq_along( unique_days ) ){
      
      # Get indices of this day in dt_mkt_month
      relevant_day <- unique_days[[ ix ]]
      idx_in_dt <- which( days == relevant_day )
      
      # What prod_delta_idx appear in this day?
      prod_delta_idx_day <- prod_delta_idx[ idx_in_dt ]
      unique_prod_delta_idx_day <- sort( unique( prod_delta_idx_day ) )
      
      # Compute daily market shares
      mkt_shares_day <- self$compute_mkt_shares_dt( dist_tau = dist_tau,
                                                    delta = delta[ idx_in_dt ],
                                                    dep_times = dep_times[ idx_in_dt ],
                                                    supp_tau = supp_tau,
                                                    kappa = kappa )
      
      # Loop over the prod_delta_idx that appear in the day aggregating predicted_shares
      for ( jx in seq_along( unique_prod_delta_idx_day ) ){
        # What are the indices (of mkt_shares_day) that correspond to the ix-th prod_delta_idx
        idx_tmp <- unique_prod_delta_idx_day[[ jx ]]
        prod_idx <- which( prod_delta_idx_day == idx_tmp )
        # Sum the corresponding market shares and add to predicted shares
        predicted_shares[[ idx_tmp ]] <- predicted_shares[[ idx_tmp ]] + sum( mkt_shares_day[ prod_idx ] )
      }
      
      # Add these market shares to the corresponding entries in `predicted_shares`
      #predicted_shares[ prod_delta_idx_day ] <- predicted_shares[ prod_delta_idx_day ] + mkt_shares_day
      
    }
    
    # Return predicted quantities
    market_size * predicted_shares
    
  },
  
  update_delta_mkt_month = function( dt_mkt_month, dist_tau, supp_tau, kappa, debug = FALSE ){
    
    # Again, this function exists *only* for development.
    # It is coded in C++ style to facilitate conversion.
    
    if ( debug ){
      browser()
    }
    
    # What prod_delta_idx appear in the data?
    prod_delta_idx <- dt_mkt_month[ , prod_delta_idx ]
    unique_prod_delta_idx <- sort( unique( prod_delta_idx  ) )
    
    # Get the vector of delta's [Note this has to be done differently in C++ because arma::unique automatically sorts]
    unique_deltas <- dt_mkt_month[
      ,
      .( delta = delta[[ 1 ]] ),
      by = .( prod_delta_idx )
      ][
        order( prod_delta_idx )
        ][ , delta ]
    
    # Get the vector of observed daily quantities
    observed_daily_qtys <- dt_mkt_month[ , scaled_num_tix ]
    
    # Computed observed quantities
    # Initialize a vector to store those
    observed_qtys <- numeric( length( unique_prod_delta_idx ) )
    
    for ( ix in seq_along( unique_prod_delta_idx ) ){
      
      # Find the indices of the ix-th prod_delta_idx
      idx_in_dt <- which( prod_delta_idx == unique_prod_delta_idx[[ ix ]] )
      
      # Aggregate quantities
      observed_qtys[[ ix ]] <- sum( observed_daily_qtys[ idx_in_dt ] )
      
    }
    
    # Compute predicted_qtys
    predicted_qtys <- self$compute_pred_qty_mkt_month( dt_mkt_month = dt_mkt_month, dist_tau = dist_tau, supp_tau = supp_tau, kappa = kappa )
    
    # Update delta and return
    delta_new <- unique_deltas + log( observed_qtys ) - log( predicted_qtys )
    #list( prod_delta_idx = unique_prod_delta_idx, delta = unique_deltas, observed_qtys = observed_qtys, predicted_qtys = predicted_qtys, delta_new = delta_new )
    delta_new
    
  },
  
  contraction_mapping_vcpp = function( dt_mkt_month, dist_tau, supp_tau, kappa, tol, iter_max, debug = FALSE ){
    
    if ( debug ){
      browser()
    }
    
    # Deep copy dt_mkt_month
    dt_copy <- copy( dt_mkt_month )
    
    # Initial guess: vector of unique delta's in the data
    delta_current <- dt_copy[
      ,
      .( delta = delta[[ 1 ]] ),
      by = .( prod_delta_idx )
      ][
        order( prod_delta_idx )
        ][ , delta ]
    
    # Loop iteratively updating delta
    dist_delta <- 1
    iter <- 1
    while ( dist_delta >= tol && iter <= iter_max ){
      
      # Update delta's
      delta_new <- self$update_delta_mkt_month( dt_mkt_month = dt_copy, dist_tau = dist_tau, supp_tau = supp_tau, kappa = kappa )
      # Compute the distance between delta's
      dist_delta <- max( abs( delta_current - delta_new ) )
      # Update delta_current
      delta_current <- delta_new
      # Change delta in dt_copy
      dt_copy[ , delta := delta_current[ prod_delta_idx ] ]
      # Update iteration counter
      iter <- iter + 1
      
    }
    
    if ( dist_delta >= tol ){
      stop( "Contraction mapping vcpp failed to converge." )
    }
    
    # Return dt_copy, now with the new delta.
    #dt_copy
    
    # Return a list with two vectors: prod_delta_idx and delta
    list( prod_delta_idx = dt_copy[ , sort( unique( prod_delta_idx ) ) ],
          delta = delta_current
          )
    
  },
  
  contraction_mapping = function( dt_tmp, kappa, supp_tau, dist_tau, tol, iter_max, debug = FALSE ){
    
    # dt_tmp here is a market-month level dataset.
    
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
        daily_mkt_shares := self$compute_mkt_shares_dt( dist_tau = dist_tau,
                                                        delta = delta,
                                                        dep_times = time_of_day,
                                                        supp_tau = supp_tau,
                                                        kappa = kappa
        ),
        by = .( data_viagem )
        ][
          ,
          .(
            delta = delta[[ 1 ]], # unique within the group by construction
            total_qty = sum( scaled_num_tix ),
            predicted_qty = sum( daily_mkt_shares ) * market_size[[ 1 ]] # Market size is constant within the group by construction
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
    
  },
  
  estimate_delta_omega = function( kappa, dt_tmp, supp_tau, tol, tol_contraction, return_omega = FALSE, verbose = FALSE, debug = FALSE ){
    
    # >>> Comments <<<
      # This function estimates delta and omega as a function of kappa.
      # It also computes the resulting sum of squared residuals.
    
    if ( debug ){
      browser()
    }
    
    # Deep copy the argument.
    dt_tmp_copy <- copy( dt_tmp )
    
    # Compute delta
    dt_tmp_copy[
      ,
      delta := log( mkt_share ) - log( 1 - sum( mkt_share ) ),
      by = .( market, data_viagem )
      ][
        ,
        delta := mean( delta ),
        by = .( market, cnpj, tipo_servico_novo, ano_viagem, mes_viagem )
        ]
    
    # Support length
    length_supp <- length( supp_tau )
    
    # # Define prod_delta_idx for all months
    # dt_tmp_copy[
    #   ,
    #   prod_delta_idx := frank( paste( cnpj, tipo_servico_novo ), ties.method = "dense" ),
    #   by = .( market, ano_viagem, mes_viagem )
    #   ]
    
    # Delta convergence bool
    conv_delta <- FALSE
    
    # >>> Some objects that don't change <<<
    # FKRB inputs
    B_ls <- dt_tmp_copy[ , mkt_share ]
    E_ls <- matrix( data = 1, nrow = 1, ncol = length_supp )
    F_ls <- 1
    G_ls <- diag( length_supp )
    H_ls <- rep( 0, length_supp )
    
    while( !conv_delta ){
      
      # Store current value/guess of delta
      delta_current <- dt_tmp_copy[
        ,
        .( delta = delta[[ 1 ]] ), # delta constant within group.
        by = .( market, ano_viagem, mes_viagem, prod_delta_idx )
        ][ , delta ]
      
      # Compute FKRB mats for all markets (days)
      list_fkrb_mats <- dt_tmp_copy[
        ,
        .(
          fkrb_mat = list( self$compute_ccps_dt( delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = kappa ) )
        ),
        by = .( market, data_viagem )
        ][
          ,
          fkrb_mat
          ]
      
      # Estimate the tau distribution by FKRB
      # Constrained least squares using limSolve::lsei
      A_ls <- do.call( what = rbind, args = list_fkrb_mats )
      
      estimate <- limSolve::lsei( A = A_ls, B = B_ls, E = E_ls, F = F_ls, G = G_ls, H = H_ls,
                                  type = 2 )
      
      dist_tau <- estimate$X
      
      # Solve for delta's from constraints *separately month by month*
      # >>> First try <<<
      # First it failed because .SD can't be modified in place
      # Now it works because I'm deep-copying the argument `dt_tmp_copy`
      dt_tmp_copy <- dt_tmp_copy[
        ,
        self$contraction_mapping( dt_tmp = .SD, kappa = kappa, supp_tau = supp_tau, dist_tau = dist_tau, tol = tol_contraction, iter_max = 100 ),
        by = .( market, ano_viagem, mes_viagem )
        ]
      # >>> First try <<<
      
      # Check for convergence
      delta_new <- dt_tmp_copy[
        ,
        .( delta = delta[[ 1 ]] ), # delta constant within group.
        by = .( market, ano_viagem, mes_viagem, prod_delta_idx )
        ][ , delta ]
      dist_delta <- max( abs( delta_current - delta_new ) )
      conv_delta <- ( dist_delta < tol )
      if ( verbose ){
        cat( "dist_delta = ", dist_delta, ".\n", sep = "" )
      }
      
    }
    
    # Compute the resulting market shares *and* the SSRs
    ssr <- dt_tmp_copy[
      ,
      daily_mkt_shares := self$compute_mkt_shares_dt( dist_tau = dist_tau,
                                                      delta = delta,
                                                      dep_times = time_of_day,
                                                      supp_tau = supp_tau,
                                                      kappa = kappa
      ),
      by = .( market, data_viagem )
    ][
      ,
      sum( ( mkt_share - daily_mkt_shares )^2 )
    ]
    
    if ( return_omega ){
      list( ssr = ssr, omega = dist_tau )
    } else {
      ssr
    }
    
  },
  
  estimate_delta_omega_vcpp = function( kappa, dt_tmp, supp_tau, tol, tol_contraction, return_omega = FALSE, verbose = FALSE, debug = FALSE ){
    
    # Version of estimate_delta_omega using C++ functions.
    
    if ( debug ){
      browser()
    }
    
    # Deep copy the argument.
    dt_tmp_copy <- copy( dt_tmp )
    
    # Support length
    length_supp <- length( supp_tau )
    
    # Delta convergence bool
    conv_delta <- FALSE
    
    # >>> Some objects that don't change <<<
    # FKRB inputs
    B_ls <- dt_tmp_copy[ , mkt_share ]
    E_ls <- matrix( data = 1, nrow = 1, ncol = length_supp )
    F_ls <- 1
    G_ls <- diag( length_supp )
    H_ls <- rep( 0, length_supp )
    
    while( !conv_delta ){
      
      # Store current value/guess of delta
      delta_current <- dt_tmp_copy[
        ,
        .( delta = delta[[ 1 ]] ), # delta constant within group.
        keyby = .( market, ano_viagem, mes_viagem, prod_delta_idx )
        ][ , delta ]
      
      # Compute FKRB mats for all markets (days)
      list_fkrb_mats <- dt_tmp_copy[
        ,
        .(
          fkrb_mat = list( cpp_compute_ccps_dt( delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = kappa ) )
        ),
        by = .( market, data_viagem )
        ][
          ,
          fkrb_mat
          ]
      
      # Estimate the tau distribution by FKRB
      # Constrained least squares using limSolve::lsei
      A_ls <- do.call( what = rbind, args = list_fkrb_mats )
      
      estimate <- limSolve::lsei( A = A_ls, B = B_ls, E = E_ls, F = F_ls, G = G_ls, H = H_ls,
                                  type = 2 )
      
      dist_tau <- estimate$X
      
      # Solve for delta's from constraints *separately month by month*
      # >>> First try <<<
      # First it failed because .SD can't be modified in place
      # Now it works because I'm deep-copying the argument `dt_tmp_copy`
      dt_delta_new <- dt_tmp_copy[
        ,
        cpp_contraction_mapping( dt_mkt_month = as.matrix( .SD[ , .( dia_viagem, prod_delta_idx, delta, time_of_day, market_size, scaled_num_tix ) ] ),
                                 dist_tau = dist_tau, supp_tau = supp_tau, kappa = kappa, tol = tol_contraction, iter_max = 100 ),
        keyby = .( market, ano_viagem, mes_viagem )
        ]
      # >>> First try <<<
      
      # Check for convergence
      delta_new <- dt_delta_new[ , delta ]
      dist_delta <- max( abs( delta_current - delta_new ) )
      conv_delta <- ( dist_delta < tol )
      if ( verbose ){
        cat( "dist_delta = ", dist_delta, ".\n", sep = "" )
      }
      
      # Update delta in dt_tmp_copy
      dt_tmp_copy[ , delta := NULL ]
      dt_tmp_copy <- dt_delta_new[ dt_tmp_copy, on = c( "market", "ano_viagem", "mes_viagem", "prod_delta_idx" ) ]
      
    }
    
    # Compute the resulting market shares *and* the SSRs
    ssr <- dt_tmp_copy[
      ,
      daily_mkt_shares := cpp_compute_mkt_shares_dt( dist_tau = dist_tau,
                                                     delta = delta,
                                                     dep_times = time_of_day,
                                                     supp_tau = supp_tau,
                                                     kappa = kappa
                                                     ),
      by = .( market, data_viagem )
      ][
        ,
        sum( ( mkt_share - daily_mkt_shares )^2 )
        ]
    
    if ( return_omega ){
      list( ssr = ssr, omega = dist_tau )
    } else {
      ssr
    }
    
  },
  
  compute_mkt_posterior_dt = function( omega, mat_ccps, delta, dep_times, market_shares, supp_tau, kappa, debug = FALSE ){
    
    # >>> Comments <<<
      # This is a silly function bc the EM boils down to matrix operations
    
    # >>> Arguments <<<
      # omega is the (current guess of) the distribution of \tau.
      # mat_ccps is a J x M matrix. Entry (j,m) is the probability that j is chosen by type m.
    
    if ( debug ){
      browser()
    }
    
    # Number of products
    num_prods <- length( dep_times ) + 1 # + 1 for the outside option
    # Number of points of support
    length_support <- length( supp_tau )
    
    # Compute matrix of CCPs (products on rows, consumer types on columns)
    #mat_ccps <- self$compute_ccps_dt( delta = delta, dep_times = dep_times, supp_tau = supp_tau, kappa = kappa, return_ccp_out_option = TRUE )
    
    # Weight CCPs by current distribution of tau
    weighted_ccps <- mat_ccps * matrix( data = omega, nrow = num_prods, ncol = length_support, byrow = TRUE )
    
    # Compute denominators of posterior distribution of \tau
    unconditional_probs <- rowSums( weighted_ccps )
    
    # Compute posterior probabilities
    posterior_dist <- weighted_ccps / matrix( data = unconditional_probs, nrow = num_prods, ncol = length_support )
    
    # Compute the market posterior and return
    c( 1 - sum( market_shares ), market_shares ) %*% posterior_dist
    
  },
  
  compute_em = function( omega_0, ccp_mat_stacked, vec_num_tix,
                         tol = 1e-03, iter_max = 1e+06, verbose = FALSE, debug = FALSE ){
    
    # >>> Arguments <<<
      # omega_0 is the initial guess for the \omega distribution
    
    if ( debug ){
      browser()
    }
    
    omega <- omega_0
    conv <- FALSE
    
    num_prods_total <- nrow( ccp_mat_stacked )
    length_support <- length( omega )
    total_tix <- sum( vec_num_tix )
    iter <- 1
    
    while( !conv && iter <= iter_max ){
      
      # Weight ccps
      weighted_ccps <- ccp_mat_stacked * matrix( data = omega, nrow = num_prods_total, ncol = length_support, byrow = TRUE )
      
      # Compute denominators of posterior distribution of \tau
      unconditional_probs <- rowSums( weighted_ccps )
      
      # Compute posterior probabilities
      posterior_dist <- weighted_ccps / matrix( data = unconditional_probs, nrow = num_prods_total, ncol = length_support )
      
      # Update the distribution
      omega_new <- num_tix %*% posterior_dist / total_tix
      
      # Check convergence
      dist <- max( abs( omega_new - omega ) )
      conv <- ( dist < tol )
      
      # Print message
      if ( verbose && ( ( iter %% 25 ) == 0 ) ){
        cat( "dist = ", dist, ".\n", sep = "" )
      }
      
      # Update omega
      omega <- omega_new
      iter <- iter + 1
      
    }
    
    # Return the result
    if ( conv ){
      omega
    } else {
      stop( "EM algorithm failed to converge.\n" )
    }
    
  }
  
)

)