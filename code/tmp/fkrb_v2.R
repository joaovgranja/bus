# Implementing FKRB, using more data.

# Clean-up, packages, classes and functions -------------------------------

rm( list = ls() )

library( data.table )
library( tidyverse )
library( microbenchmark )
library( parallel )

source( "code/estimation/R6_fkrb.R" )
fkrb_inst <- fkrb$new()
rm( fkrb )

# Load data ---------------------------------------------------------------

dt_tix <- fread( input = "clean_data/dt_tix_working.gz",
                 keepLeadingZeros = TRUE,
                 integer64 = "character",
                 nThread = 4L
)

dt_op <- fread( input = "clean_data/dt_op_consolidated.gz",
                keepLeadingZeros = TRUE,
                integer64 = "character",
                nThread = 4L
)

dt_pop <- readRDS( "clean_data/demographics/pop_data.Rds" )
setDT( dt_pop )

# Focus on largest markets ----------------------------------------------------------

# Find the 10 (arbitrary) largest markets in dt_tix
relevant_markets <- dt_tix[
  ,
  .N,
  by = .( market_linha_no_order )
][
  order( desc( N ) )
][
  1:10,
  market_linha_no_order
]

relevant_markets

# Tix
dt_tix_tmp <- dt_tix[ market_linha_no_order %in% relevant_markets ]
rm( dt_tix )
gc()

# Keep only tickets for the full route
dt_tix_tmp <- dt_tix_tmp[ market_no_order == market_linha_no_order ]

# Operational
dt_op_tmp <- dt_op[
  market_linha_no_order %in% relevant_markets
  ]
rm( dt_op )
gc()
#

# Construct dt_products: daily data of products and qtys ----------------------------------------------------

# Get all products (market,firm,date,service,time) and qtys
dt_products <- dt_tix_tmp[
  ,
  .(
    num_tix = .N
  ),
  .( market, code_ibge_line_origin, code_ibge_line_destination, cnpj, data_viagem, tipo_servico_novo, hora_viagem )
  ][
    order( data_viagem, tipo_servico_novo, hora_viagem )
    ]

dt_products

# Compute time of departure, scaled to [0,1]
dt_products[
  ,
  hora_viagem := as.ITime( hora_viagem )
  ][
    ,
    `:=`(
      hour_of_day = hour( hora_viagem ),
      mins = minute( hora_viagem )
    )
    ][
      ,
      `:=`(
        time_of_day = ( hour_of_day * 60 + mins ) / ( 24 * 60 ),
        hour_of_day = NULL,
        mins = NULL
      )
      ][]
#

# Add prices dt_products --------------------------------------------------

dt_prices <- dt_tix_tmp[
  ,
  .( avg_price = mean( valor_tarifa_com_desconto, trim = 0.025 ) ),
  by = .( market, cnpj, ano_viagem, mes_viagem, tipo_servico_novo )
  ]

dt_prices

# Get ano_viagem and mes_viagem in dt_products to merge
dt_products[
  ,
  `:=`(
    ano_viagem = year( data_viagem ),
    mes_viagem = month( data_viagem )
  )
  ][]

# Merge dt_prices onto dt_products
dt_products <- merge.data.table( x = dt_products,
                                 y = dt_prices,
                                 by = c( "market", "cnpj", "ano_viagem", "mes_viagem", "tipo_servico_novo" )
)

# Look at an example
dt_products[
  market == "BELO HORIZONTE(MG)-GUARAPARI(ES)" & data_viagem == "2019-09-25"
  ]
#

# Use dt_op to correct qtys -----------------------------------------------

# Quantities in the OP dataset
dt_qtys_op <- dt_op_tmp[
  market_no_order == market_linha_no_order & data_ymd >= "2019-01-01",
  .( num_tix = sum( numeroPagantes ) ),
  by = .( market, cnpj, tipoDeServicoPrincipal, data_ymd )
  ]

dt_qtys_op

# Quantities in the Tix dataset
dt_qty_mes <- dt_tix_tmp[
  ,
  .N,
  by = .( market, cnpj, tipo_servico_novo, ano_viagem, mes_viagem )
]

dt_qty_mes

# Merge OP qtys onto tix qtys
str( dt_qty_mes )
str( dt_qtys_op )
# Create dates and make them Date types for the merge
dt_qty_mes[ , data_ymd := as.IDate( paste( ano_viagem, mes_viagem, "01", sep = "-" ) ) ][]
dt_qtys_op[ , data_ymd := as.IDate( data_ymd ) ][]

dt_qty_mes <- merge.data.table( x = dt_qty_mes,
                                y = dt_qtys_op,
                                by.x = c( "market", "cnpj", "tipo_servico_novo", "data_ymd" ),
                                by.y = c( "market", "cnpj", "tipoDeServicoPrincipal", "data_ymd" ),
                                all.x = TRUE
)

dt_qty_mes

# Compute weights
dt_qty_mes[
  ,
  weight := pmax( 1, num_tix / N ) # if dt_tix has more tickets, trust that
  ][]

dt_qty_mes[ , quantile( weight, seq( 0, 1, 0.1 ), na.rm = TRUE ) ]

# Some weights will be missing bc there's no OP data for 2020
  # assume they are the same as the Dec 2019 weights
# First get the last non NA weight
dt_qty_mes[
  ,
  last_non_na_weight := {
    tmp <- weight[ !is.na( weight ) ]
    if ( length( tmp ) == 0 ){
      NA_real_
    } else {
      tmp[[ length( tmp ) ]]
    }
  },
  by = .( market, cnpj, tipo_servico_novo )
][]

# Note that doesn't work in some cases
dt_qty_mes[ is.na( last_non_na_weight ) ]

# Fix the cases that are fixable
dt_qty_mes[
  ,
  weight := ifelse( is.na( weight ), last_non_na_weight, weight )
][]

# Drop all pairs (market,month) that have an NA
dt_qty_mes <- dt_qty_mes[
  ,
  if ( all( !is.na( num_tix[ ano_viagem == 2019 ] ) ) ) .SD,
  by = .( market, data_ymd )
]
# Could improve this with assumptions, but for now let's do this.
# This ano_viagem == 2019 is wrong. This will return all the 2020 data.

# Merge these onto dt_products
dt_products <- merge.data.table( x = dt_products,
                                 y = dt_qty_mes[ , .( market, cnpj, tipo_servico_novo, ano_viagem, mes_viagem, weight ) ],
                                 by = c( "market", "cnpj", "tipo_servico_novo", "ano_viagem", "mes_viagem" ),
                                 all.x = TRUE
)

# Check markets with no weight
dt_products[ is.na( weight ) ] # quite a good chunk. Again, can improve with assumptions, but use this for now.

# Keep only the (market,month) pairs s.t. all products have weights.
dt_products <- dt_products[
  ,
  if ( all( !is.na( weight ) ) ) .SD,
  by = .( market, ano_viagem, mes_viagem )
]

dt_products[ , quantile( weight, probs = seq( 0, 1, 0.1 ) ) ]
dt_products[ , quantile( weight, probs = seq( 0.9, 1, 0.01 ) ) ]
# Some crazy weights. Maybe consistent with previous plots, though.

# Scale the tickets by the weights
dt_products[
  ,
  scaled_num_tix := round( num_tix * weight )
  ][]
# Look at the resulting distribution
dt_products[ , quantile( scaled_num_tix, probs = seq( 0, 1, 0.1 ) ) ]
dt_products[ , quantile( scaled_num_tix, probs = seq( 0.9, 1, 0.01 ) ) ]
# Most of it ends up being reasonable. Some of it is quite unreasonable.
# What if you restrict attention to "entirely reasonable" (daily) markets?
# That really cuts into the data by a lot.
dt_products[
  ,
  if ( all( scaled_num_tix <= 125 ) ) .SD,
  by = .( market, ano_viagem, mes_viagem )
]

# For now go with this
dt_products <- dt_products[
  ,
  if ( all( scaled_num_tix <= 125 ) ) .SD,
  by = .( market, ano_viagem, mes_viagem )
  ]
#

# Define market size and compute mkt shares -------------------------------

# Add population onto dt_products.
# Keep only 2019 population
dt_pop <- dt_pop[ year == 2019 ]
# Make sure types of IBGE codes are the same.
str( dt_pop )
str( dt_products )
dt_products[
  ,
  `:=`(
    code_ibge_line_origin = as.character( code_ibge_line_origin ),
    code_ibge_line_destination = as.character( code_ibge_line_destination )
  )
][]
# Merge origin population
dt_products <- merge.data.table( x = dt_products,
                                 y = dt_pop[ , .( code_munic, pop_origin = pop_estimate ) ],
                                 by.x = "code_ibge_line_origin",
                                 by.y = "code_munic" )
dt_products
# Merge destination population
dt_products <- merge.data.table( x = dt_products,
                                 y = dt_pop[ , .( code_munic, pop_destination = pop_estimate ) ],
                                 by.x = "code_ibge_line_destination",
                                 by.y = "code_munic" )

dt_products

# Compute market size
dt_products[
  ,
  market_size := sqrt( pop_origin ) * sqrt( pop_destination ) / 365
][]

# Compute market shares. Drop market size and population.
dt_products[
  ,
  `:=`(
    mkt_share = scaled_num_tix / market_size,
    #market_size = NULL, # Market size is needed to compute quantities below
    pop_origin = NULL,
    pop_destination = NULL
  )
  ][]

dt_products[ , quantile( mkt_share, seq( 0, 1, 0.1 ) ) ]
#

# Example: compute FKRB matrix for a single (market, day) --------------------------------------------------------------

dt_products[
  market == "BELO HORIZONTE(MG)-GUARAPARI(ES)" & data_viagem == "2019-09-25"
  ]

# Use this example
dt_products_tmp <- dt_products[
  market == "BELO HORIZONTE(MG)-GUARAPARI(ES)" & data_viagem == "2019-09-25"
  ]

# Compute a reasonable delta
dt_products_tmp[
  ,
  delta := log( mkt_share ) - log( 1 - sum( mkt_share ) )
  ][
    ,
    delta := mean( delta ),
    by = .( market, cnpj, tipo_servico_novo, ano_viagem, mes_viagem )
    ][]

# Product chars
prod_chars <- t( as.matrix( dt_products_tmp[ , .( delta, time_of_day ) ] ) )

# Support
supp_tau <- seq( from = 0, to = 1, length.out = 40 )

# Compute CCPs:
mat_ccps <- fkrb_inst$compute_ccps( prod_chars = prod_chars, supp_tau = supp_tau )
# Test the function passing delta and locations directly
test <- fkrb_inst$compute_ccps_dt( delta = as.numeric( prod_chars[ 1, ] ), dep_times = as.numeric( prod_chars[ 2, ] ), supp_tau = supp_tau, debug = TRUE )
identical( mat_ccps, test ) # TRUE
#

# Get FKRB matrix for all pairs (market,day) ----------------------------------

dt_products[
  ,
  delta := log( mkt_share ) - log( 1 - sum( mkt_share ) ),
  by = .( market, data_viagem )
  ][
    ,
    delta := mean( delta ),
    by = .( market, cnpj, tipo_servico_novo, ano_viagem, mes_viagem )
    ][]

# Do this directly in data.table
supp_tau <- seq( 0, 1, length.out = 40L )

list_fkrb_mats <- dt_products[
  ,
  .(
    fkrb_mat = list( fkrb_inst$compute_ccps_dt( delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 10 ) )
  ),
  by = .( market, data_viagem )
  ][
    ,
    fkrb_mat
    ]
#

# Estimate distribution of tau --------------------------------------------

length_supp <- length( supp_tau )

# Estimating the distribution of tau using FKRB
# Constrained least squares using limSolve::lsei
A_ls <- do.call( what = rbind, args = list_fkrb_mats )
B_ls <- dt_products[ , mkt_share ]
E_ls <- matrix( data = 1, nrow = 1, ncol = length_supp )
F_ls <- 1
G_ls <- diag( length_supp )
H_ls <- rep( 0, length_supp )

estimate <- limSolve::lsei( A = A_ls, B = B_ls, E = E_ls, F = F_ls, G = G_ls, H = H_ls,
                            type = 2 )

estimate
#

# Example: Compute market shares for a single day ---------------------------------------------------

dist_tau <- estimate$X

fkrb_inst$compute_mkt_shares( dist_tau = dist_tau, prod_chars = prod_chars,supp_tau = supp_tau, kappa = 10, debug = TRUE )
#

# Example: Dealing with the constraint for a specific market-month ---------------------------------------------

# Create data for example
dt_tmp <- dt_products[
  market == "BELO HORIZONTE(MG)-GUARAPARI(ES)" & ano_viagem == 2019 & mes_viagem == 9
  ][
    order( market, data_viagem, cnpj, tipo_servico_novo, hora_viagem )
    ][
      ,
      prod_delta_idx := frank( paste( cnpj, tipo_servico_novo ), ties.method = "dense" ),
      by = .( market, ano_viagem, mes_viagem )
      ]

# Check
dt_tmp
unique( dt_tmp[ , .( prod_delta_idx, delta ) ] )

# Compute daily predicted market shares, compute predicted and actual qtys, update delta.
dt_tmp[
  ,
  daily_mkt_shares := fkrb_inst$compute_mkt_shares_dt( dist_tau = dist_tau, delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 10 ),
  by = .( market, data_viagem )
  ][][
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
      ][]

# Loop using the code above
dist <- 1
iter <- 1
while( dist > 1e-04 && iter <= 25 ){
  
  # Compute shares, observed and predicted quantity and update delta
  dt_delta_new <- dt_tmp[
    ,
    daily_mkt_shares := fkrb_inst$compute_mkt_shares_dt( dist_tau = dist_tau, delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 10 ),
    by = .( market, data_viagem )
    ][
      ,
      .(
        delta = delta[[ 1 ]], # unique within the group by construction
        total_qty = sum( scaled_num_tix ),
        predicted_qty = sum( daily_mkt_shares ) * market_size[[ 1 ]]
      ),
      by = .( prod_delta_idx )
      ][
        ,
        delta_new := delta + log( total_qty ) - log( predicted_qty )
        ][]
  
  # Compute distance
  dist <- dt_delta_new[ , max( abs( delta - delta_new ) ) ]
  # Clean dt_delta_new
  dt_delta_new <- dt_delta_new[ , .( prod_delta_idx, delta = delta_new ) ]
  
  # Join the new delta onto dt_tmp
  dt_tmp[ , delta := NULL ]
  dt_tmp <- dt_delta_new[ dt_tmp, on = "prod_delta_idx" ]
  
  # Print comforting message
  cat( "Distance is equal to ", dist, ".\n", sep = "" )
  
  iter <- iter + 1
  
}

# Check that function gives same result
fkrb_inst$contraction_mapping( dt_tmp = dt_tmp, supp_tau = supp_tau, dist_tau = dist_tau, kappa = 10, tol = 1e-04, iter_max = 25, debug = FALSE )
# why do I have no delta here anymore? Must be something about modifying in place, but I couldn't figure it out.
#

# Preliminaries: delta, support of tau, and product idxs -----------------------------------------------------------

# Compute delta
dt_products[
  ,
  delta := log( mkt_share ) - log( 1 - sum( mkt_share ) ),
  by = .( market, data_viagem )
  ][
    ,
    delta := mean( delta ),
    by = .( market, cnpj, tipo_servico_novo, ano_viagem, mes_viagem )
    ][]

# Define the support of tau
supp_tau <- seq( from = 0, to = 1, length.out = 40 )
# Support length
length_supp <- length( supp_tau )

# Define prod_delta_idx for all months
dt_products[
  ,
  prod_delta_idx := frank( paste( cnpj, tipo_servico_novo ), ties.method = "dense" ),
  by = .( market, ano_viagem, mes_viagem )
  ][]
#

# Put all the pieces together ---------------------------------------------

# Delta convergence bool
conv_delta <- FALSE

# >>> Some objects that don't change <<<
# FKRB inputs
B_ls <- dt_products[ , mkt_share ]
E_ls <- matrix( data = 1, nrow = 1, ncol = length_supp )
F_ls <- 1
G_ls <- diag( length_supp )
H_ls <- rep( 0, length_supp )

while( !conv_delta ){
  
  # Store current value/guess of delta
  delta_current <- dt_products[ , unique( delta ) ]
  
  # Compute FKRB mats for all markets (days)
  list_fkrb_mats <- dt_products[
    ,
    .(
      fkrb_mat = list( fkrb_inst$compute_ccps_dt( delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 12 ) )
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
  # Now it works because I'm deep-copying the argument `dt_tmp`
  dt_products <- dt_products[
    ,
    fkrb_inst$contraction_mapping( dt_tmp = .SD, kappa = 12, supp_tau = supp_tau, dist_tau = dist_tau, tol = 1e-04, iter_max = 100 ),
    by = .( market, ano_viagem, mes_viagem )
    ]
  # >>> First try <<<
  
  # >>> Second try <<<
  # Get lists of DTs, one for each month
  # list_dts_months <- dt_products[
  #   ,
  #   .( dt_month = list( .SD ) ),
  #   by = .( market, ano_viagem, mes_viagem )
  #   ][
  #     ,
  #     dt_month
  #     ]
  # Apply `contraction_mapping` to each DT in the list
  # test <- lapply( X = list_dts_months, fkrb_inst$contraction_mapping, tol = 1e-04, iter_max = 100 )
  # >>> Second try <<<
  
  # Check for convergence
  dist_delta <- max( abs( delta_current - dt_products[ , unique( delta ) ] ) )
  conv_delta <- ( dist_delta < 1e-04 )
  cat( "dist_delta = ", dist_delta, ".\n", sep = "" )
  
}

dist_tau
#

# Test estimation of delta,omega ------------------------------------------

supp_tau <- seq( from = 0, to = 1, length.out = 40 )
fkrb_inst$estimate_delta_omega( kappa = 12, dt_tmp = dt_products, supp_tau = supp_tau,
                                tol = 1e-04, tol_contraction = 1e-04,
                                return_omega = TRUE, verbose = TRUE, debug = FALSE )

# Initialize cluster
num_cores <- 4L
my_cluster <- makeCluster( num_cores, type = "FORK" )

# Compute the objective function at a grid of \kappa values
vec_values_objective <- parLapply( cl = my_cluster,
                                   X = 1:20,
                                   fun = fkrb_inst$estimate_delta_omega,
                                   dt_tmp = dt_products,
                                   supp_tau = supp_tau,
                                   tol = 1e-04,
                                   tol_contraction = 1e-04,
                                   return_omega = FALSE,
                                   verbose = FALSE,
                                   debug = FALSE
                                   )

vec_values_objective
plot( 1:20, vec_values_objective ) # =OOO
# Plot it nicely
data.table(
  kappa = 1:20,
  ssr = unlist( vec_values_objective )
) %>% 
  ggplot( aes( x = kappa, y = ssr ) ) +
  geom_line( size = 0.75 ) +
  ggtitle( "SSR As a Function of kappa" )

# Look at it in [3,6]
vec_values_objective_2 <- parLapply( cl = my_cluster,
                                   X = seq( 3, 6, length.out = 10L ),
                                   fun = fkrb_inst$estimate_delta_omega,
                                   dt_tmp = dt_products,
                                   supp_tau = supp_tau,
                                   tol = 1e-04,
                                   tol_contraction = 1e-04,
                                   return_omega = FALSE,
                                   verbose = FALSE,
                                   debug = FALSE
)

plot( seq( 3, 6, length.out = 10L ), vec_values_objective_2 ) # =OOO
# Plot it nicely
data.table(
  kappa = seq( 3, 6, length.out = 10L ),
  ssr = unlist( vec_values_objective_2 )
) %>% 
  ggplot( aes( x = kappa, y = ssr ) ) +
  geom_line( size = 0.75 ) +
  ggtitle( "SSR As a Function of kappa" )

# Take 4 1/3 as the estimate. What's the resulting distribution?
fkrb_inst$estimate_delta_omega( kappa = 4 + 1/3, dt_tmp = dt_products, supp_tau = supp_tau,
                                tol = 1e-04, tol_contraction = 1e-04,
                                return_omega = TRUE, verbose = TRUE, debug = FALSE )

#ggsave( "output/plot_ssr_kappa.png" )

# What's the resulting distribution (for now at kappa = 12)?
dist_estimated <- fkrb_inst$estimate_delta_omega( kappa = 12, dt_tmp = dt_products, supp_tau = supp_tau, return_omega = TRUE, debug = FALSE )
dist_estimated$omega
# Plot it.
data.table(
  supp_tau = supp_tau,
  weight = dist_estimated$omega
) %>% 
  ggplot( aes( x = supp_tau, y = weight ) ) +
  geom_bar( stat = "identity", fill = "bisque4" ) +
  ggtitle( "Estimated Distribution of Ideal Departure Time" ) +
  xlab( "Ideal Departure Time" ) +
  ylab( "Probability Mass" )

#ggsave( "output/plot_tau_dist.png" )

# Example: calculation of market posterior ------------------------------------

# Use this example
dt_products_tmp <- dt_products[
  market == "BELO HORIZONTE(MG)-GUARAPARI(ES)" & data_viagem == "2019-09-25"
  ]

# Compute a reasonable delta
dt_products_tmp[
  ,
  delta := log( mkt_share ) - log( 1 - sum( mkt_share ) )
  ][
    ,
    delta := mean( delta ),
    by = .( market, cnpj, tipo_servico_novo, ano_viagem, mes_viagem )
    ][]

fkrb_inst$compute_mkt_posterior_dt( omega = rep( 1 / length( supp_tau ) ),
                                    delta =  )

test <- dt_products_tmp[
  ,
  fkrb_inst$compute_mkt_posterior_dt( omega = rep( 1 / length( supp_tau ), times = length( supp_tau ) ),
                                      delta = delta,
                                      dep_times = time_of_day,
                                      market_shares = mkt_share,
                                      supp_tau = supp_tau,
                                      kappa = 10,
                                      debug = FALSE
                                      )
]

sum(test)

# Example: Compute posterior once ------------------------------

dist_tau <- rep( 1 / length( supp_tau ), times = length( supp_tau ) )

market_sizes <- dt_products[
  ,
  .( market_size = market_size[[ 1 ]] ), # constant within group by construction
  by = .( market, data_viagem )
][
  ,
  market_size
]

list_posteriors <- dt_products[
  ,
  .(
    mkt_posterior = list( fkrb_inst$compute_mkt_posterior_dt( omega = dist_tau,
                                                              delta = delta,
                                                              dep_times = time_of_day,
                                                              market_shares = mkt_share,
                                                              supp_tau = supp_tau,
                                                              kappa = 10,
                                                              debug = FALSE
    ) )
  ),
  by = .( market, data_viagem )
][
  ,
  mkt_posterior
]

omega_posterior <- purrr::map2( .x = market_sizes, .y = list_posteriors, .f = function( x, y ){ x * y } )
omega_posterior <- purrr::reduce( omega_posterior, `+` ) / sum( market_sizes )

# Example: compute all CCP matrices -----------------------------------------

# This is the FKRB matrix with the outside option as well
list_ccp_mats <- dt_products[
  ,
  .(
    mat_ccps = list( fkrb_inst$compute_ccps_dt( delta = delta,
                                                dep_times = time_of_day,
                                                supp_tau = supp_tau,
                                                kappa = 10,
                                                return_ccp_out_option = TRUE
    )
    )
  ),
  by = .( market, data_viagem )
  ][
    ,
    mat_ccps
  ]

ccp_mat_stacked <- do.call( what = "rbind", args = list_ccp_mats )

# Example: update dist using stacked CCP mat ------------------------------

weighted_ccps <- ccp_mat_stacked * matrix( data = dist_tau, nrow = nrow( ccp_mat_stacked ), ncol = length( dist_tau ), byrow = TRUE )

# Compute denominators of posterior distribution of \tau
unconditional_probs <- rowSums( weighted_ccps )

# Compute posterior probabilities
posterior_dist <- weighted_ccps / matrix( data = unconditional_probs, nrow = nrow( ccp_mat_stacked ), ncol = length( dist_tau ) )

# Get vector of market shares
list_mkt_shares <- dt_products[
  ,
  .(
    market_shares = list( c( 1 - sum( mkt_share ), mkt_share ) )
  ),
  by = .( market, data_viagem )
][
  ,
  market_shares
]

market_shares <- unlist( list_mkt_shares )

# Get vector of number of tickets
list_num_tix <- dt_products[
  ,
  .(
    num_tix = list( c( round( market_size[[ 1 ]] ) - sum( scaled_num_tix ), scaled_num_tix ) )
  ),
  by = .( market, data_viagem )
  ][
    ,
    num_tix
    ]

num_tix <- unlist( list_num_tix )

# Compute the market posterior and return
num_tix %*% posterior_dist / sum( num_tix )
# So easy!

# Check EM implementation
test <- fkrb_inst$compute_em( omega_0 = dist_tau, ccp_mat_stacked = ccp_mat_stacked, vec_num_tix = num_tix, tol = 1e-06, debug = FALSE, verbose = TRUE )
saveRDS( )