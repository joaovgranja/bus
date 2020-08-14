# BH-SP, preparing for demand estimation

# Clean-up, packages, classes and functions -------------------------------

rm( list = ls() )

library( data.table )
library( tidyverse )
library( microbenchmark )

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

# Focus on a single market, explore the data ----------------------------------------------------------

# Tix
dt_tix_tmp <- dt_tix[ market_linha_no_order == "BELO HORIZONTE(MG)-SAO PAULO(SP)" ]
rm( dt_tix )
gc()

# Operational
dt_op_tmp <- dt_op[
  market_linha_no_order == "BELO HORIZONTE(MG)-SAO PAULO(SP)"
  ]
rm( dt_op )
gc()

# Population
dt_pop_tmp <- dt_pop[
  year == 2019 & ( name_munic == "Belo Horizonte" | str_detect( name_munic, "^S.o Paulo$" ) )
  ]
rm( dt_pop )
gc()

# Submarkets (i.e., both directions)
submkts <- dt_tix_tmp[
  market_no_order == market_linha_no_order & data_viagem >= "2019-01-01",
  unique( market )
  ]

submkts

# Monthly qty from TIX dt
dt_qty_mes <- dt_tix_tmp[
  market == submkts[[ 1 ]]
  ][
    ,
    .N,
    by = .( cnpj, ano_viagem, mes_viagem, tipo_servico_novo )
    ][
      ,
      data_ymd := as.IDate( paste( ano_viagem, mes_viagem, "01", sep = "-" ) )
      ] 

# Plot qty from TIX dt
dt_qty_mes %>% 
  ggplot( aes( x = data_ymd, y = N, col = tipo_servico_novo, group = tipo_servico_novo ) ) +
  geom_line() +
  facet_wrap( ~ cnpj, ncol = 2L )

# Look at low numbers
dt_qty_mes[
  cnpj == "61084018000103"
  ][
    order( ano_viagem, mes_viagem )
    ]

# Plot number of tickets per trip
dt_tix_tmp[
  market_no_order == market_linha_no_order & market == submkts[[ 1 ]]
  ][
    ,
    .N,
    by = .( cnpj, tipo_servico_novo, data_viagem, hora_viagem )
    ][
      ,
      .( avg_passengers = mean( N ) ),
      by = .( cnpj, tipo_servico_novo, data_viagem )
      ][
        ,
        .( avg_passengers = mean( avg_passengers ) ),
        by = .( cnpj, tipo_servico_novo, year( data_viagem ), month( data_viagem ) )
        ][
          ,
          data_ymd := as.IDate( paste( year, month, "01", sep = "-" ) )
          ] %>% 
  ggplot( aes( x = data_ymd, y = avg_passengers, color = tipo_servico_novo ) ) +
  geom_line() +
  facet_wrap( ~ cnpj, ncol = 2L )

# Does each codigo_viagem correspond to a unique type of service?
dt_tix[
  ,
  if ( length( unique( tipo_servico_novo ) ) > 1 ) .SD,
  by = .( codigo_viagem.y )
  ] # 0 obs -> YES.

# Redo plot above using codigo_viagem.y instead of service/time combinations
dt_tix_tmp[
  market_no_order == market_linha_no_order & market == submkts[[ 1 ]]
  ][
    ,
    .N,
    by = .( cnpj, tipo_servico_novo, data_viagem, codigo_viagem.y )
    ][
      ,
      .( avg_passengers = mean( N ) ),
      by = .( cnpj, tipo_servico_novo, data_viagem )
      ][
        ,
        .( avg_passengers = mean( avg_passengers ) ),
        by = .( cnpj, tipo_servico_novo, year( data_viagem ), month( data_viagem ) )
        ][
          ,
          data_ymd := as.IDate( paste( year, month, "01", sep = "-" ) )
          ] %>% 
  ggplot( aes( x = data_ymd, y = avg_passengers, color = tipo_servico_novo ) ) +
  geom_line() +
  facet_wrap( ~ cnpj, ncol = 2L )
# Same result

# Construct dt_products: daily data of products and qtys ----------------------------------------------------

# Get all products (market,firm,date,service,time) and qtys
dt_products <- dt_tix_tmp[
  market == submkts[[ 1 ]],
  .(
    num_tix = .N
  ),
  .( market, cnpj, data_viagem, tipo_servico_novo, hora_viagem )
  ][
    order( data_viagem, tipo_servico_novo, hora_viagem )
    ]

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

# Add prices dt_products --------------------------------------------------

dt_prices <- dt_tix_tmp[
  market == submkts[[ 1 ]],
  .( avg_price = mean( valor_tarifa_com_desconto, trim = 0.025 ) ),
  by = .( cnpj, ano_viagem, mes_viagem, tipo_servico_novo )
]

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
                                 by = c( "cnpj", "ano_viagem", "mes_viagem", "tipo_servico_novo" )
                                 )

# Look at an example
dt_products[
  data_viagem == "2019-09-25"
]

# Use dt_op to correct qtys -----------------------------------------------

# Quantities in the OP dataset
dt_qtys_op <- dt_op_tmp[
  market_no_order == market_linha_no_order & data_ymd >= "2019-01-01" & market == submkts[[ 1 ]],
  .( num_tix = sum( numeroPagantes ) ),
  by = .( cnpj, tipoDeServicoPrincipal, data_ymd )
]

# Quantities in the Tix dataset
dt_qty_mes

# Merge OP qtys onto tix qtys
str( dt_qty_mes )
str( dt_qtys_op )
dt_qtys_op[ , data_ymd := as.IDate( data_ymd ) ][]

dt_qty_mes <- merge.data.table( x = dt_qty_mes,
                  y = dt_qtys_op,
                  by.x = c( "cnpj", "tipo_servico_novo", "data_ymd" ),
                  by.y = c( "cnpj", "tipoDeServicoPrincipal", "data_ymd" ),
                  all = TRUE
                  )

dt_qty_mes

# Compute weights
dt_qty_mes[
  ,
  weight := num_tix / N
][]

# Correct the NAs in `weight` getting the last non-NA value
dt_qty_mes[
  ,
  weight := ifelse( !is.na( weight ), weight, weight[ !is.na( weight ) ][[ .N - 2 ]] ), # Horrible!!!
  by = .( cnpj, tipo_servico_novo )
][]

# Merge these onto dt_products
dt_products <- merge.data.table( x = dt_products,
                                 y = dt_qty_mes[ , .( cnpj, tipo_servico_novo, ano_viagem, mes_viagem, weight ) ],
                                 by = c( "cnpj", "tipo_servico_novo", "ano_viagem", "mes_viagem" )
)

# Scale tickets by the appropriate weight
dt_products[
  ,
  scaled_num_tix := round( num_tix * weight )
][]

# Define market size and compute mkt shares -------------------------------

# Market size
mkt_size <- dt_pop_tmp[
  ,
  prod( sqrt( pop_estimate ) ) / 365
]

# Compute market shares
dt_products[
  ,
  mkt_share := scaled_num_tix / mkt_size
][]

# Example: compute FKRB matrix for a single day --------------------------------------------------------------

dt_products[
  data_viagem == "2019-09-25"
  ]

# Use this example
dt_products_tmp <- dt_products[
  data_viagem == "2019-09-25"
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
supp_tau <- seq( from = 0, to = 1, length.out = 20 )

# Compute CCPs:
mat_ccps <- fkrb_inst$compute_ccps( prod_chars = prod_chars, supp_tau = supp_tau )
# Test the function passing delta and locations directly
test <- fkrb_inst$compute_ccps_dt( delta = as.numeric( prod_chars[ 1, ] ), dep_times = as.numeric( prod_chars[ 2, ] ), supp_tau = supp_tau )
identical( mat_ccps, test ) # TRUE
#

# Get FKRB matrix for all markets (days) ----------------------------------

dt_products[
  ,
  delta := log( mkt_share ) - log( 1 - sum( mkt_share ) ),
  by = .( market, data_viagem )
][
  ,
  delta := mean( delta ),
  by = .( market, cnpj, tipo_servico_novo, ano_viagem, mes_viagem )
  ][]

# Get product characteristics for each market
list_prod_chars <- dt_products[
  ,
  list( prod_chars = list( t( as.matrix( .SD[ , .( delta, time_of_day ) ] ) ) ) ),
  by = .( market, data_viagem )
  ][
    ,
    prod_chars
    ]

# Check
list_prod_chars[[ 1 ]]

# Define the support of tau
supp_tau <- seq( from = 0, to = 1, length.out = 20 )

# Compute CCPs for all prod_chars matrices
list_fkrb_mats_2 <- lapply( X = list_prod_chars, FUN = fkrb_inst$compute_ccps, supp_tau = supp_tau, kappa = 1 )

# Do this directly in data.table
list_fkrb_mats <- dt_products[
  ,
  .(
    fkrb_mat = list( fkrb_inst$compute_ccps_dt( delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 1 ) )
  ),
  by = .( market, data_viagem )
][
  ,
  fkrb_mat
]

identical( list_fkrb_mats, list_fkrb_mats_2 ) # TRUE
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

# Example: Compute market shares for a single day ---------------------------------------------------

dist_tau <- estimate$X

fkrb_inst$compute_mkt_shares( dist_tau = dist_tau, prod_chars = prod_chars,supp_tau = supp_tau, kappa = 1 )

# Example: Dealing with the constraint for Sep 2019 ---------------------------------------------

# Create data for example
dt_tmp <- dt_products[
  ano_viagem == 2019 & mes_viagem == 9
][
  order( market, data_viagem, cnpj, tipo_servico_novo, hora_viagem )
][
  ,
  prod_delta_idx := frank( paste( cnpj, tipo_servico_novo ), ties.method = "dense" ),
  by = .( market, ano_viagem, mes_viagem )
][]

# Check
dt_tmp

# Compute daily predicted market shares, compute predicted and actual qtys, update delta.
dt_tmp[
  ,
  daily_mkt_shares := fkrb_inst$compute_mkt_shares_dt( dist_tau = dist_tau, delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 1 ),
  by = .( market, data_viagem )
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
    ][]

# Loop using the code above
dist <- 1
iter <- 1
while( dist > 1e-04 && iter <= 25 ){
  
  # Compute shares, observed and predicted quantity and update delta
  dt_delta_new <- dt_tmp[
    ,
    daily_mkt_shares := fkrb_inst$compute_mkt_shares_dt( dist_tau = dist_tau, delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 1 ),
    by = .( market, data_viagem )
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
fkrb_inst$contraction_mapping( dt_tmp = dt_tmp, tol = 1e-04, iter_max = 25, debug = FALSE )
dt_tmp # why do I have no delta here anymore? Must be something about modifying in place, but I couldn't figure it out.
#


# Preliminaries -----------------------------------------------------------

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
supp_tau <- seq( from = 0, to = 1, length.out = 20 )

# Put all the pieces together ---------------------------------------------

# Define prod_delta_idx for all months
dt_products[
  ,
  prod_delta_idx := frank( paste( cnpj, tipo_servico_novo ), ties.method = "dense" ),
  by = .( market, ano_viagem, mes_viagem )
][]

# Delta convergence bool
conv_delta <- FALSE

# >>> Some objects that don't change <<<
# Support length
length_supp <- length( supp_tau )
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
      fkrb_mat = list( fkrb_inst$compute_ccps_dt( delta = delta, dep_times = time_of_day, supp_tau = supp_tau, kappa = 20 ) )
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
    fkrb_inst$contraction_mapping( dt_tmp = .SD, tol = 1e-04, iter_max = 100 ),
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
