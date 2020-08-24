# C++ tests

library( Rcpp )
library( RcppArmadillo )
library( data.table )
library( microbenchmark )
library( tidyverse )
library( parallel )

# Inline tests ------------------------------------------------------------

cppFunction( code = "
             Rcpp::List myfun( arma::vec myvec ){
              return Rcpp::List::create( Rcpp::NumericVector( myvec.begin(), myvec.end() ) );
             }", depends = "RcppArmadillo" )

myfun( 1.1 * 1:5 )

# Load C++ file and corresponding R class ---------------------------------

# C++
sourceCpp( file = "code/estimation/cpp_fkrb.cpp" )

# R
source( "code/estimation/R6_fkrb.R" )
fkrb_inst <- fkrb$new()
rm( fkrb )

# Load data to source examples --------------------------------------------

dt_products <- fread( input = "clean_data/dt_fkrb_full_route_only.csv",
                      keepLeadingZeros = TRUE,
                      integer64 = "character"
                      )

dt_products

# Clean data (move this to the file preparing data) -----------------------

dt_products[
  ,
  dia_viagem := as.integer( str_extract( string = data_viagem, pattern = "\\d{2}$" ) )
  ][]

# Compute an initial delta, used throughout -------------------------------

dt_products[
  ,
  delta := log( mkt_share ) - log( share_out_option ),
  by = .( market, data_viagem )
][
  ,
  delta := mean( delta ),
  by = .( market, data_ym01, prod_delta_idx )
][]
#

# Define supp_tau, used throughout ----------------------------------------

supp_tau <- seq( 0, 1, length.out = 40L )

# Test calculation of CCP mat ---------------------------------------------

dt_tmp <- dt_products[ market == "BRASILIA(DF)-ANAPOLIS(GO)" & data_viagem == "2019-08-25" ]
dt_tmp

# R
mat_r <- dt_tmp[
  ,
  fkrb_inst$compute_ccps_dt( delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 10 )
]

# C++
mat_cpp <- dt_tmp[
  ,
  cpp_compute_ccps_dt( delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 10 )
  ]

identical( mat_r, mat_cpp )
max( abs( mat_r - mat_cpp ) ) # 1e-18

# Any difference in speed?
microbenchmark( R = dt_tmp[
  ,
  fkrb_inst$compute_ccps_dt( delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 10 )
  ],
  cpp = dt_tmp[
    ,
    cpp_compute_ccps_dt( delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 10 )
    ] )
# C++ is a bit faster.

# Test on the entire dataset.
# C++
test1 <- dt_products[
  ,
  .(
    fkrb_mat = list( cpp_compute_ccps_dt( delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 10 ) )
  ),
  by = .( market, data_viagem )
  ][
    ,
    fkrb_mat
    ]
test1 <- do.call( what = "rbind", args = test1 )
# R
test2 <- dt_products[
  ,
  .(
    fkrb_mat = list( fkrb_inst$compute_ccps_dt( delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 10 ) )
  ),
  by = .( market, data_viagem )
  ][
    ,
    fkrb_mat
    ]
test2 <- do.call( what = "rbind", args = test2 )
# Same dimension?
dim( test1 )
dim( test2 )
# Yes.
# Check their difference
max( abs( test1 - test2 ) ) # Machine epsilon.
#

# Test calculation of market shares ---------------------------------------

dist_tau <- rep( 1 / length( supp_tau ), times = length( supp_tau ) )

# R.
dt_tmp[
  ,
  fkrb_inst$compute_mkt_shares_dt( dist_tau = dist_tau, delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 10 )
]

# C++.
dt_tmp[
  ,
  cpp_compute_mkt_shares_dt( dist_tau = dist_tau, delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 10 )
]

# Test on the entire data
test1 <- dt_products[
  ,
  .( mkt_share = fkrb_inst$compute_mkt_shares_dt( dist_tau = dist_tau, delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 10 ) ),
  by = .( market, data_viagem )
]

test2 <- dt_products[
  ,
  .( mkt_share = as.numeric( cpp_compute_mkt_shares_dt( dist_tau = dist_tau, delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 10 ) ) ),
  by = .( market, data_viagem )
  ]

head( test1 )
head( test2 )
max( abs( test1[ , mkt_share ] - test2[ , mkt_share ] ) ) # Machine epsilon.
#

# Developing contraction in C++ -------------------------------------------

# Get a market-month example
dt_products
dt_products[
  ,
  if ( length( unique( prod_delta_idx ) ) > 1 ) .SD,
  by = .( market, data_ym01 )
]
dt_tmp <- dt_products[
  market == "SAO LOURENCO(MG)-BRASILIA(DF)" & data_ym01 == "2020-02-01"
]
dt_tmp

# Compute predicted qtys within data.table to know what I wanna get.
dt_tmp[
  ,
  daily_mkt_shares := fkrb_inst$compute_mkt_shares_dt( dist_tau = dist_tau, delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 10 ),
  by = .( dia_viagem )
  ][
    ,
    .(
      delta = delta[[ 1 ]], # unique within the group by construction
      total_qty = sum( scaled_num_tix ),
      predicted_qty = sum( daily_mkt_shares ) * market_size[[ 1 ]] # Market size is constant within the group by construction
    ),
    by = .( prod_delta_idx )
    ]

# Developing a function to compute market shares at the month-product level [C++ style code]
# >>> NOTE: this works for this example but not in general. Functions have been corrected already. <<<
# Get unique days
unique_days <- dt_tmp[ , sort( unique( dia_viagem ) ) ]

# Initialize predicted shares
predicted_shares <- numeric( length = dt_tmp[ , length( unique( prod_delta_idx ) ) ] )

for ( ix in seq_along( unique_days ) ){
  
  # Get data for the day
  dt_day <- dt_tmp[ dia_viagem == unique_days[[ ix ]] ]
  
  # Get prod_delta_idx for this day
  prod_delta_idx_tmp <- dt_day[ , prod_delta_idx ]
  
  # Compute mkt shares for this day
  mkt_shares_tmp <- dt_day[
    ,
    fkrb_inst$compute_mkt_shares_dt( dist_tau = dist_tau, delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 10 )
    ]
  
  # Add to the predicted shares vector
  predicted_shares[ prod_delta_idx_tmp ] <- predicted_shares[ prod_delta_idx_tmp ] + mkt_shares_tmp
  
}

# Compute predicted qty
dt_tmp[ , unique( market_size ) ]
predicted_qty <- predicted_shares * dt_tmp[ , market_size[[ 1 ]] ]
predicted_qty
# >>> Same result <<<

# Computing observed qtys (C++ style)
dt_tmp
observed_qtys <- numeric( dt_tmp[ , length( unique( prod_delta_idx ) ) ] )
for ( ix in seq_len( nrow( dt_tmp ) ) ){
  
  prod_delta_idx_tmp <- dt_tmp[ ix, prod_delta_idx ]
  observed_qtys[ prod_delta_idx_tmp ] <- observed_qtys[ prod_delta_idx_tmp ] + dt_tmp[ ix, scaled_num_tix ]
  
}
# Check
observed_qtys
dt_tmp[ , .( sum( scaled_num_tix ) ), by = .( prod_delta_idx ) ] # Yep.

# How to get unique values of delta (C++ style)?

# Test function computing predicted qty
# Desired result
predicted_qty
# R
fkrb_inst$compute_pred_qty_mkt_month( dt_mkt_month = dt_tmp, dist_tau = dist_tau, supp_tau = supp_tau, kappa = 10, debug = FALSE )
# C++
cpp_compute_pred_qty_mkt_month( dt_mkt_month = as.matrix( dt_tmp[ , .( dia_viagem, prod_delta_idx, delta, time_of_day, market_size ) ] ),
                                dist_tau = dist_tau, supp_tau = supp_tau, kappa = 10 )
# Same result

# Test on the entire data
test1 <- dt_products[
  ,
  daily_mkt_shares := fkrb_inst$compute_mkt_shares_dt( dist_tau = dist_tau, delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 10 ),
  by = .( market, data_viagem )
  ][
    ,
    .(
      delta = delta[[ 1 ]], # unique within the group by construction
      total_qty = sum( scaled_num_tix ),
      predicted_qty = sum( daily_mkt_shares ) * market_size[[ 1 ]] # Market size is constant within the group by construction
    ),
    by = .( market, ano_viagem, mes_viagem, prod_delta_idx )
    ][
      order( market, ano_viagem, mes_viagem, prod_delta_idx )
    ]

test2 <- dt_products[
  ,
  .( predicted_qty = fkrb_inst$compute_pred_qty_mkt_month( dt_mkt_month = .SD, dist_tau = dist_tau, supp_tau = supp_tau, kappa = 10 ) ),
  by = .( market, ano_viagem, mes_viagem )
][
  order( market, ano_viagem, mes_viagem )
]

test3 <- dt_products[
  ,
  .( predicted_qty = as.numeric( cpp_compute_pred_qty_mkt_month( dt_mkt_month = as.matrix( .SD[ , .( dia_viagem, prod_delta_idx, delta, time_of_day, market_size, scaled_num_tix ) ] ),
                                                     dist_tau = dist_tau, supp_tau = supp_tau, kappa = 10 ) )
     ),
  by = .( market, ano_viagem, mes_viagem )
  ][
    order( market, ano_viagem, mes_viagem )
  ]

test1
test2
test3
max( abs( test3[ , predicted_qty ] - test1[ , predicted_qty ] ) ) # 1e-12
test1[ 20069 ]
test2[ 20069 ]
test3[ 20069 ]
# R and C++ function are delivering the same *incorrect* result. Debug R.
# Redefine dt_tmp as one of the problematic cases
dt_tmp <- dt_products[
  market == "FLORIANOPOLIS(SC)-SAO PAULO(SP)" & ano_viagem == 2020 & mes_viagem == 1
]
dt_tmp[
  ,
  daily_mkt_shares := fkrb_inst$compute_mkt_shares_dt( dist_tau = dist_tau, delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 10 ),
  by = .( data_viagem )
  ][
    ,
    .(
      delta = delta[[ 1 ]], # unique within the group by construction
      total_qty = sum( scaled_num_tix ),
      predicted_qty = sum( daily_mkt_shares ) * market_size[[ 1 ]] # Market size is constant within the group by construction
    ),
    by = .( market, ano_viagem, mes_viagem, prod_delta_idx )
    ][
      order( market, ano_viagem, mes_viagem, prod_delta_idx )
      ]
fkrb_inst$compute_pred_qty_mkt_month( dt_mkt_month = dt_tmp, dist_tau = dist_tau, supp_tau = supp_tau, kappa = 10, debug = FALSE )
cpp_compute_pred_qty_mkt_month( dt_mkt_month = as.matrix( dt_tmp[ , .( dia_viagem, prod_delta_idx, delta, time_of_day, market_size, scaled_num_tix ) ] ),
                                dist_tau = dist_tau, supp_tau = supp_tau, kappa = 10 )
#

# Test function updating delta --------------------------------------------

# data.table
dt_tmp[
  ,
  daily_mkt_shares := fkrb_inst$compute_mkt_shares_dt( dist_tau = dist_tau, delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 10 ),
  by = .( dia_viagem )
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
      ][ order( prod_delta_idx ) ]

# R function
fkrb_inst$update_delta_mkt_month( dt_mkt_month = dt_tmp, dist_tau = dist_tau, supp_tau = supp_tau, kappa = 10, debug = FALSE )
# C++ function
cpp_update_delta_mkt_month( dt_mkt_month = as.matrix( dt_tmp[ , .( dia_viagem, prod_delta_idx, delta, time_of_day, market_size, scaled_num_tix ) ] ),
                            dist_tau = dist_tau, supp_tau = supp_tau, kappa = 10
                            )
# Same result

# >>> Test it on the entire data <<<
# 1. data.table
test1 <- dt_products[
  ,
  daily_mkt_shares := fkrb_inst$compute_mkt_shares_dt( dist_tau = dist_tau, delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 10 ),
  by = .( market, data_viagem )
  ][
    ,
    .(
      delta = delta[[ 1 ]], # unique within the group by construction
      total_qty = sum( scaled_num_tix ),
      predicted_qty = sum( daily_mkt_shares ) * market_size[[ 1 ]] # Market size is constant within the group by construction
    ),
    by = .( market, ano_viagem, mes_viagem, prod_delta_idx )
    ][
      ,
      delta_new := delta + log( total_qty ) - log( predicted_qty )
      ][ order( market, ano_viagem, mes_viagem, prod_delta_idx ) ]
# R.
test2 <- dt_products[
  ,
  fkrb_inst$update_delta_mkt_month( dt_mkt_month = .SD, dist_tau = dist_tau, supp_tau = supp_tau, kappa = 10, debug = FALSE ),
  by = .( market, ano_viagem, mes_viagem )
][ order( market, ano_viagem, mes_viagem, prod_delta_idx ) ]
# Check distance bw test1, test2
max( abs( test1[ , delta_new ] - test2[ , delta_new ] ) ) # 1e-15
# C++
test3 <- dt_products[
  ,
  as.numeric( cpp_update_delta_mkt_month( dt_mkt_month = as.matrix( .SD[ , .( dia_viagem, prod_delta_idx, delta, time_of_day, market_size, scaled_num_tix ) ] ),
                              dist_tau = dist_tau, supp_tau = supp_tau, kappa = 10
  ) ),
  by = .( market, ano_viagem, mes_viagem )
  ][ order( market, ano_viagem, mes_viagem ) ]

test2
test3
max( abs( test2[ , delta_new ] - test3[ , V1 ] ) ) # 1e-15
#

# Test contraction mapping -----------------------------------------------------

dt_tmp

# R.
fkrb_inst$contraction_mapping_vcpp( dt_mkt_month = dt_tmp, dist_tau = dist_tau, supp_tau = supp_tau, kappa = 10, tol = 1e-04, iter_max = 100, debug = FALSE )
# C++.
cpp_contraction_mapping( dt_mkt_month = as.matrix( dt_tmp[ , .( dia_viagem, prod_delta_idx, delta, time_of_day, market_size, scaled_num_tix ) ] ),
                         dist_tau = dist_tau, supp_tau = supp_tau, kappa = 10, tol = 1e-04, iter_max = 100
)

# Test contraction mapping on all the possible inputs ---------------------

# Get all possible inputs
list_dt_mkt_month <- dt_products[
  order( market, ano_viagem, mes_viagem, dia_viagem, prod_delta_idx, delta, time_of_day, market_size, scaled_num_tix )
][
  ,
  .(
    dt_mkt_month = list( as.matrix( .SD[ , .( dia_viagem, prod_delta_idx, delta, time_of_day, market_size, scaled_num_tix ) ] ) )
  ),
  by = .( market, ano_viagem, mes_viagem )
][
  , dt_mkt_month
]

for ( ix in seq_along( list_dt_mkt_month ) ){
  
  test <- cpp_contraction_mapping( dt_mkt_month = list_dt_mkt_month[[ ix ]],
                                   dist_tau = dist_tau, supp_tau = supp_tau, kappa = 10, tol = 1e-04, iter_max = 100
  )
  
  if ( ix %% 1000 == 0 ){
    cat( "Iteration ", ix, ".\n", sep = "" )
  }
  
}

# No errors.
# Call it on the whole data to see the output
test <- dt_products[
  ,
  cpp_contraction_mapping( dt_mkt_month = as.matrix( .SD[ , .( dia_viagem, prod_delta_idx, delta, time_of_day, market_size, scaled_num_tix ) ] ),
                           dist_tau = dist_tau, supp_tau = supp_tau, kappa = 10, tol = 1e-04, iter_max = 100
  ),
  by = .( market, ano_viagem, mes_viagem )
]

test
test[ is.na( delta ) ] # Beautiful

# Testing `estimate` using C++ functions ----------------------------------

fkrb_inst$estimate_delta_omega_vcpp( kappa = 10, dt_tmp = dt_products, supp_tau = supp_tau,
                                     tol = 1e-04, tol_contraction = 1e-04,
                                     return_omega = TRUE, verbose = TRUE, debug = FALSE
                                     )

# Apply `estimate` to a \kappa-grid, in parallel ----------------------------------------------

kappa_grid <- 1:20

# Initialize cluster
num_cores <- 4L
my_cluster <- makeCluster( num_cores, type = "FORK" )

vec_values_objective <- parLapply( cl = my_cluster,
                                   X = kappa_grid,
                                   fun = fkrb_inst$estimate_delta_omega_vcpp,
                                   dt_tmp = dt_products, supp_tau = supp_tau,
                                   tol = 1e-04, tol_contraction = 1e-04,
                                   return_omega = FALSE, verbose = FALSE, debug = FALSE
                                   )
