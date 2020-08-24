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

dt_products <- fread( input = "clean_data/dt_fkrb_full_route_only.csv",
                      keepLeadingZeros = TRUE,
                      integer64 = "character"
)

# Create dia_viagem (move this to the file preparing data) -----------------------

dt_products[
  ,
  dia_viagem := as.integer( str_extract( string = data_viagem, pattern = "\\d{2}$" ) )
  ][]

# Call --------------------------------------------------------------------

# Get task_id
task_id <- as.integer( Sys.getenv( "SGE_TASK_ID" ) )

# Call `estimate` with kappa = task_id
output <- fkrb_inst$estimate_delta_omega_vcpp( kappa = task_id, dt_tmp = dt_products, supp_tau = supp_tau,
                                     tol = 1e-04, tol_contraction = 1e-04,
                                     return_omega = TRUE, verbose = FALSE, debug = FALSE
)

# Save --------------------------------------------------------------------

saveRDS( object = output, file = paste( "output/estimate_delta_omega/edo_kappa_", task_id, ".Rds", sep = "" ) )
