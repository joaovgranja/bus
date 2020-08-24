# Task array call of estimate_delta_omega_vcpp

rm( list = ls() )

library( Rcpp )
library( RcppArmadillo )
library( data.table )
library( microbenchmark )
library( tidyverse )
library( parallel )

# Load C++ file and corresponding R class ---------------------------------

# C++
sourceCpp( file = "code/estimation/cpp_fkrb.cpp" )

# R
source( "code/estimation/R6_fkrb.R" )
fkrb_inst <- fkrb$new()
rm( fkrb )

# Load data --------------------------------------------

dt_products <- fread( input = "clean_data/dt_fkrb_gontijo.csv",
                      keepLeadingZeros = TRUE,
                      integer64 = "character"
)

# if `scaled_num_tix` is not available, create it (in that case this is the gontijo dataset)
if ( !( "scaled_num_tix" %in% names( dt_products ) ) ){
  dt_products[ , scaled_num_tix := num_tix ]
}

# Create dia_viagem (move this to the file preparing data) -----------------------

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

# Call --------------------------------------------------------------------

# Get task_id
task_id <- as.integer( Sys.getenv( "SGE_TASK_ID" ) )

# Call `estimate` with kappa = task_id
output <- fkrb_inst$estimate_delta_omega_vcpp( kappa = task_id, dt_tmp = dt_products, supp_tau = supp_tau,
                                     tol = 1e-04, tol_contraction = 1e-04,
                                     return_omega = TRUE, verbose = FALSE, debug = FALSE
)

# Save --------------------------------------------------------------------

saveRDS( object = output, file = paste( "output/estimate_delta_omega/edo_kappa_gontijo", task_id, ".Rds", sep = "" ) )
