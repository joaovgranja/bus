# Prepare data for FKRB estimation

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

# Construct dt_products: daily data of product and qtys -------------------

# Get all products (market,firm,date,service,time) and qtys
dt_products <- dt_tix[
  market_no_order == market_linha_no_order,
  .(
    num_tix = .N
  ),
  .( market, code_ibge_line_origin, code_ibge_line_destination, cnpj, data_viagem, tipo_servico_novo, hora_viagem )
  ][
    order( data_viagem, tipo_servico_novo, hora_viagem )
    ]

dt_products
#

# Compute the time of departure, scaled to [0,1] --------------------------

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

# Compute monthly quantities in dt_tix and dt_op --------------------------

dt_qty_tix <- dt_tix[
  market_no_order == market_linha_no_order,
  .(
    .N,
    preco = mean( valor_tarifa_com_desconto, trim = 0.025, na.rm = TRUE )
    ),
  by = .( market, cnpj, ano_viagem, mes_viagem, tipo_servico_novo )
]

dt_qty_op <- dt_op[
  market_no_order == market_linha_no_order & data_ymd >= "2019-01-01",
  .( num_tix = sum( numeroPagantes ) ),
  by = .( market, cnpj, data_ymd, tipoDeServicoPrincipal )
][
  num_tix > 0
  ]
#

# Merge dt_qty_op onto dt_qty_tix -----------------------------------------

str( dt_qty_tix )
str( dt_qty_op )
# Create data_ymd in dt_tix for the merge
dt_qty_tix[
  ,
  data_ymd := paste( ano_viagem,
                     str_pad( string = mes_viagem, width = 2L, side = "left", pad = "0" ),
                     "01",
                     sep = "-"
                     )
][]

dt_qty_mes <- merge.data.table( x = dt_qty_tix,
                                y = dt_qty_op,
                                by.x = c( "market", "cnpj", "tipo_servico_novo", "data_ymd" ),
                                by.y = c( "market", "cnpj", "tipoDeServicoPrincipal", "data_ymd" ),
                                all = TRUE
)

dt_qty_mes

# Compute weights (and fix 2020 weights) ----------------------------------

dt_qty_mes[
  ,
  weight := pmax( 1, num_tix / N ) # if dt_tix has more tickets, trust that
][]

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
dt_qty_mes[ , sum( is.na( weight ) ) ]
dt_qty_mes[
  ,
  `:=`(
    weight = ifelse( is.na( weight ), last_non_na_weight, weight ),
    last_non_na_weight = NULL
  )
  ][]
dt_qty_mes[ , sum( is.na( weight ) ) ]
#

# Keep only markets with complete weights ---------------

dt_qty_mes <- dt_qty_mes[
  ,
  if ( all( !is.na( weight ) ) ) .SD,
  by = .( market, data_ymd )
]
#

# Merge prices and weights onto dt_products ------------------------------------------

str( dt_products )
str( dt_qty_mes )
# Add data_ym01 to dt_products
dt_products[
  ,
  data_viagem := as.IDate( data_viagem )
  ][
    ,
    `:=`(
      ano_viagem = year( data_viagem ), # I use ano_viagem and mes_viagem in the estimation code
      mes_viagem = month( data_viagem ),
      data_ym01 = paste( year( data_viagem ),
                         str_pad( string = month( data_viagem ), width = 2L, side = "left", pad = "0" ),
                         "01",
                         sep = "-"
      )
    )
    ][]

dt_products <- merge.data.table( x = dt_products,
                                 y = dt_qty_mes[ , .( market, cnpj, data_ymd, tipo_servico_novo, preco, weight ) ],
                                 by.x = c( "market", "cnpj", "data_ym01", "tipo_servico_novo" ),
                                 by.y = c( "market", "cnpj", "data_ymd", "tipo_servico_novo" ),
                                 all = TRUE
)

dt_products

# Keep only market-month pairs with complete price and weight information
dt_products <- dt_products[
  ,
  if ( all( !is.na( preco ) ) && all( !is.na( weight ) ) ) .SD,
  by = .( market, data_ym01 )
  ]
#

# Scale num_tix -----------------------------------------------------------

dt_products[
  ,
  scaled_num_tix := ceiling( weight * num_tix )
][]

dt_products[ , quantile( scaled_num_tix, seq( 0, 1, 0.1 ) ) ]
dt_products[ , quantile( scaled_num_tix, seq( 0.9, 1, 0.01 ) ) ]

# Keep only market-month pairs with reasonable tickets --------------------

dt_products <- dt_products[
  ,
  if ( all( scaled_num_tix <= 100 ) ) .SD,
  by = .( market, data_ym01 )
]

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
dt_products[ , quantile( mkt_share, seq( 0.9, 1, 0.01 ) ) ]
# Some large values

# Compute the share of the outside option and select data -----------------

dt_products[
  ,
  share_out_option := 1 - sum( mkt_share ),
  by = .( market, data_viagem )
][]

# Drop all market-month pairs that ever have share_out_option < 0 [drops a very small share of the data]
dt_products <- dt_products[
  ,
  if ( all( share_out_option > 0 ) ) .SD,
  by = .( market, data_ym01 )
]
#

# Define delta_product_idx ------------------------------------------------

# Define prod_delta_idx for all months
dt_products[
  ,
  prod_delta_idx := frank( paste( cnpj, tipo_servico_novo ), ties.method = "dense" ),
  by = .( market, ano_viagem, mes_viagem )
  ]
#

# Save the data -----------------------------------------------------------

fwrite( x = dt_products, file = "clean_data/dt_fkrb_full_route_only.csv" )

#




# Test estimation --------------------------------------------------------------------

supp_tau <- seq( from = 0, to = 1, length.out = 40L )
fkrb_inst$estimate_delta_omega( kappa = 10, dt_tmp = dt_products, supp_tau = supp_tau, tol = 1e-03, tol_contraction = 1e-04, return_omega = TRUE, verbose = TRUE, debug = TRUE )
