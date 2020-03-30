# This script cleans the duplicates in data_tickets

# This script looks into dynamic price discrimination

# Clean-up, packages, classes and functions -------------------------------

rm( list = ls() )

library( tidyverse )
library( data.table )
library( lubridate )
library( readxl )


# Check files, define dates -------------------------------------------------------------

list.files( "raw_data/monitriip/bilhetes_de_passagem", full.names = TRUE )
list.files( "raw_data/monitriip/viagens", full.names = TRUE )

dates <- c( paste0( 0, 1:9, "_2019" ),
            paste0( 10:12, "_2019" ),
            "01_2020" )
#

# Load data -----------------------------------------------------------

# >>>>> Price data for September/2019 <<<<<
data_tickets <- fread( paste0( "raw_data/monitriip/bilhetes_de_passagem/venda_passagem_", dates[[ 9 ]], ".csv" ), integer64 = "character" )
data_tickets
str( data_tickets )

# >>>>> Trip data, September 2019 <<<<<
#data_trips <- fread( paste0( "raw_data/monitriip/viagens/viagem_regular_", dates[[ 1 ]], ".csv" ), integer64 = "character" )
#data_trips
data_trips <- fread( "output/dt_trips_od_no_duplicates.csv" )
data_trips <- data_trips[ lubridate::month( lubridate::dmy( data_viagem_programada ) ) == 9 ]

data_trips_sept_raw <- fread( "raw_data/monitriip/viagens/viagem_regular_09_2019.csv", integer64 = "character" )

# >>>>> Cadaster data, April 2019 [most recent data] <<<<<
#data_cad <- read_xlsx( path = "raw_data/dados_cadastrais/2019/04_abril/04_cadastroOperacionalServicoRegular.xlsx", skip = 1 )
#setDT( data_cad )

# >>>>> Hours data, April 2019 [most recent data]
#data_hours <- read_xlsx( path = "raw_data/dados_cadastrais/2019/04_abril/05_cadastroQuadroHorarioServicoRegular.xlsx", skip = 1 )
#setDT( data_hours )

# >>>>> Stops data, September 2019 <<<<<
#data_stops <- fread( "raw_data/monitriip/paradas/parada_regular_09_2019.csv", integer64 = "character" )
#data_stops

# Example of the problem --------------------------------------------------

# Note that this example appears in the September data

# Code below shows tickets in different directions with the same `codigo_viagem`
# data_tickets[ codigo_viagem == 2086601 ][ order( numero_poltrona ) ] %>% View()

# Check that trip
#data_trips[ codigo_viagem == 2086601 ] # Sao Paulo -> Rio trip [sentido_linha == 0]
# Look at trips by the same firm and similar `codigo_viagem`
#data_trips[ (cnpj == 60765633000112) & nu_linha == 8002800 & ( codigo_viagem %in% c( (2086601-20):(2086601+20) ) ) ][ order( codigo_viagem ) ] %>% View()
# Problem: two trips [2086600 and 2086601] with the same date/time but different directions.

# Goal: Fix `codigo_viagem` in the tickets dataset, so we know what product consumers are buying.

# Eye-balling duplicates ---------------------------------------------------

data_tickets <- unique( data_tickets, by = 2:length( data_tickets ) ) # unique on everything other than codigo_viagem
#data_trips <- unique( data_trips, by = 2:length( data_trips ) )

data_tickets[
  ,
  if ( .N > 1 ) .SD,
  by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ]
# There seem to be some duplicates due to records with categoria_transporte and tipo_servico being "Nao Se Aplica"
# I will keep only tickets with categoria_transporte == "Interestadual" below.

# Drop servico semiurbano -------------------------------------------------

data_tickets <- data_tickets[ tipo_servico != "Semiurbano" ]

# Create desconto_facultativo ---------------------------------------------

data_tickets[ , desconto_facultativo := str_detect( tipo_gratitude, "Tarifa" ) ]

# Drop international routes -----------------------------------------------

data_tickets[ , unique( categoria_transporte ) ]
data_tickets[ , summary( as.factor( categoria_transporte ) ) ]

data_tickets <- data_tickets[ categoria_transporte == "Interestadual" ]

# Check the case above again
data_tickets[ nu_linha == 19004961 ][
  ,
  if ( .N > 1 ) .SD,
  by = .( data_viagem, hora_viagem, ponto_origem_viagem, ponto_destino_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ] # No more duplicates here.

# Keeping only categoria_transporte == "Interestadual" doesn't seem to be enough.
# But every origin, destination comes with a /STATE or /COUNTRY bit
# Extract that
data_tickets[ , unique( str_extract( string = ponto_origem_viagem, pattern = "/\\w+" ) ) ]
# Drop URY, PRY and ARG
data_tickets <- data_tickets[ !( str_extract( string = ponto_origem_viagem, pattern = "/\\w+" ) %in% c( "/URY", "/PRY", "/ARG" ) ) ]
# Check
data_tickets[ , unique( str_extract( string = ponto_origem_viagem, pattern = "/\\w+" ) ) ] # Good

# Do the same with ponto_destino_viagem
data_tickets[ , unique( str_extract( string = ponto_destino_viagem, pattern = "/\\w+" ) ) ]
# Drop PRY, ARG and BOL
data_tickets <- data_tickets[ !( str_extract( string = ponto_destino_viagem, pattern = "/\\w+" ) %in% c( "/URY", "/PRY", "/ARG", "/BOL" ) ) ]
# Check
data_tickets[ , unique( str_extract( string = ponto_destino_viagem, pattern = "/\\w+" ) ) ] # Good

# Any crazy prices remaining?
data_tickets[ valor_tarifa > 2000 ] # Nope
data_tickets[ , max( valor_tarifa ) ] # 1285.09
# Check that it makes sense
data_tickets[ valor_tarifa > 1200 ] # reasonable.

# Check remaining duplicates ----------------------------------------------

data_tickets[
  ,
  if ( .N > 1 ) .SD,
  by = .( cnpj, data_viagem, hora_viagem, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ] # 127,098
# Different cases:
# 1-2. Different codigo_viagem, numero_bilhete, tipo_gratitude, valores
# 3-4. Different codigo_viagem, tipo_servico.
# 7-8 (of the 10 that print). Different codigo_viagem, tipo_servico.
# 9-10. Different codigo_viagem, tipo_servico.

# Why these different codigo_viagem? Look into it.
data_trips[
  codigo_viagem %in% c( 1981075, 1981076 )
]

data_trips_sept_raw[
  codigo_viagem %in% c( 1981075, 1981076 )
  ]
# Clear duplicate.

# Create a service index --------------------------------------------------

data_tickets[ , summary( as.factor( tipo_servico ) ) ]

data_tickets[
  ,
  servico_idx := case_when(
    str_detect( tipo_servico, "Convencional sem" ) ~ 0L,
    str_detect( tipo_servico, "Convencional com" ) ~ 1L,
    str_detect( tipo_servico, "Semileito" ) ~ 2L,
    str_detect( tipo_servico, "Leito sem" ) ~ 3L,
    str_detect( tipo_servico, "Leito com" ) ~ 4L,
    str_detect( tipo_servico, "Executivo" ) ~ 5L,
    TRUE ~ NA_integer_
  )
]

data_tickets[ , sum( is.na( servico_idx ) ) ]

# Create an obs_id, define dt_tix_duplicates and take them out ------------

# Create obs_id
data_tickets[ , obs_id := seq_len( .N ) ][]

# Define dt for duplicates
dt_tix_duplicates <- data_tickets[
  ,
  if ( .N > 1 ) .SD,
  by = .( cnpj, origem_destino_linha, data_viagem, hora_viagem, ponto_origem_viagem, ponto_destino_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ]

# Take records out of data_tickets
data_tickets <- data_tickets[ !( obs_id %in% dt_tix_duplicates$obs_id ) ]

# Clean irrelevant duplicates ---------------------------------------------

# How often is it the case that duplicates are irrelevant
# This takes a while.
dt_tix_duplicates[
  ,
  if ( all( vapply( X = .SD[ , .( tipo_servico, tipo_gratitude, valor_tarifa, valor_percentual_desconto, valor_aliquota_icms, valor_pedagio, valor_taxa_embarque, valor_total ) ],
                    FUN = function( x ){ length( unique( x ) ) },
                    FUN.VALUE = numeric( 1 )
  ) == 1
  ) ){
    .SD
  },
  by = .( cnpj, origem_destino_linha, data_viagem, hora_viagem, ponto_origem_viagem, ponto_destino_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ] # 75,324 out of 75,495!

# Define a dataset to keep these records.
dt_tix_duplicates_irrelevant <- dt_tix_duplicates[
  ,
  if ( all( vapply( X = .SD[ , .( tipo_servico, tipo_gratitude, valor_tarifa, valor_percentual_desconto, valor_aliquota_icms, valor_pedagio, valor_taxa_embarque, valor_total ) ],
                    FUN = function( x ){ length( unique( x ) ) },
                    FUN.VALUE = numeric( 1 )
  ) == 1
  ) ){
    .SD
  },
  by = .( cnpj, origem_destino_linha, data_viagem, hora_viagem, ponto_origem_viagem, ponto_destino_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ]

# Take them out of dt_tix_duplicates
dt_tix_duplicates <- dt_tix_duplicates[ !( obs_id %in% dt_tix_duplicates_irrelevant$obs_id ) ]

# Keep only the first record per group in dt_tix_duplicates_irrelevant
dt_tix_duplicates_irrelevant <- dt_tix_duplicates_irrelevant[
  ,
  .SD[ 1 ],
  by = .( cnpj, origem_destino_linha, data_viagem, hora_viagem, ponto_origem_viagem, ponto_destino_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
]

#

# What duplicates are left? -----------------------------------------------

dt_tix_duplicates[
  order( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ][ 1:2 ]
# Differences: tipo_gratitude (only the first record makes sense).
dt_tix_duplicates[
  order( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ][ 3:4 ]
# Differences: tipo_gratitude. Only the second makes sense.
dt_tix_duplicates[
  order( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ][ 5:6 ]
# Differences: codigo_viagem, tipo_servico.
dt_tix_duplicates[
  order( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ][ 7:8 ]
# Differences: codigo_viagem, tipo_servico.
dt_tix_duplicates[
  order( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ][ 9:10 ]
# Differences: codigo_viagem, tipo_servico

# Clean duplicates with multiple tipo_servico --------------------------------------------------------

# Clean cases with two types of service. Keep the best type.
dt_tix_duplicates[
  ,
  if ( length( unique( tipo_servico ) ) > 1 ) .SD,
  by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
] # 34,602

# Define a dt for these records
dt_tix_duplicates_service <- dt_tix_duplicates[
  ,
  if ( length( unique( tipo_servico ) ) > 1 ) .SD,
  by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ]

# Take these records out of dt_tix_duplicates
dt_tix_duplicates <- dt_tix_duplicates[ !( obs_id %in% dt_tix_duplicates_service$obs_id ) ]

# Keep only the record with the "best" service.
dt_tix_duplicates_service <- dt_tix_duplicates_service[
  ,
  .SD[ which.max( servico_idx ) ],
  by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ]
#

# Fix duplicates with different tipo_gratitude ----------------------------

# Define a dataset for those records
dt_tix_dup_gratitude <- dt_tix_duplicates[
  ,
  if ( length( unique( tipo_gratitude ) ) > 1 ) .SD,
  by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
][] # 10,829

# Take them out of dt_tix_duplicates
dt_tix_duplicates <- dt_tix_duplicates[ !( obs_id %in% dt_tix_dup_gratitude$obs_id ) ]

dt_tix_dup_gratitude
# Is it the case that there's always one option with tarifa normal and one with discount?
dt_tix_dup_gratitude[
  ,
  `:=`( tarifa_normal = any( str_detect( tipo_gratitude, "Normal" ) ),
        desconto = any( !str_detect( tipo_gratitude, "Normal" ) ) ),
  by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
][
  ,
  both := tarifa_normal * desconto,
][
  both == TRUE
] # 10,574, almost all of them.

# When is that not the case?
dt_tix_dup_gratitude[
  both == FALSE
]

# Keep the most informative observations
dt_tix_dup_gratitude <- dt_tix_dup_gratitude[
  ,
  if ( any( str_detect( tipo_gratitude, "Gratuidade" ) ) ){
    .SD[ str_detect( tipo_gratitude, "Gratuidade" ) == TRUE ][ 1 ]
  } else if ( any( str_detect( tipo_gratitude, "Passe Livre" ) ) ){
    .SD[ str_detect( tipo_gratitude, "Passe Livre" ) == TRUE ][ 1 ]
  } else if ( any( str_detect( tipo_gratitude, "Bilhete" ) ) ){
    .SD[ str_detect( tipo_gratitude, "Bilhete" ) == TRUE ][ 1 ]
  } else if ( any( str_detect( tipo_gratitude, "Normal" ) ) ){
    # There are some cases in which there's Tarifa Normal and Tarifa Promocional
    # I'm deciding to keep the Tarifa Normal one
    .SD[ str_detect( tipo_gratitude, "Normal" ) == TRUE ][ 1 ]
  } else {
    # Code should never get here
    .SD[ 1 ]
  },
  .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
]

dt_tix_dup_gratitude

# Clean the variables you created
dt_tix_dup_gratitude[
  ,
  `:=`( tarifa_normal = NULL,
        desconto = NULL,
        both = NULL )
]
#

# What duplicates remain? -------------------------------------------------

dt_tix_duplicates[
  order( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
][ 1:2 ]
# Differences: numero_bilhete and valor variables (one is zero).
dt_tix_duplicates[
  order( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ][ 3:4 ]
# Differences: numero_bilhete and valor variables (one is zero).
dt_tix_duplicates[
  order( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ][ 5:6 ]
# Differences: numero_bilhete and valor variables (one is zero).

# Deal with the cases with zeros ------------------------------------------

# save these cases in a dt
dt_tix_dup_zeros <- dt_tix_duplicates[
  ,
  if ( ( any( valor_tarifa < 1e-01 ) & any( valor_tarifa > 0 ) ) | ( any( valor_total < 1e-01 ) & any( valor_total > 0 ) ) ) .SD,
  by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ]

dt_tix_dup_zeros # 6,168
# I'll again choose to keep the positive one.
# These 100% discoutns might be miscoded discounts, but to think of the pricing behavior, I guess it's better to keep the "real" price

# Take records out of dt_tix_duplicates
dt_tix_duplicates <- dt_tix_duplicates[ !( obs_id %in% dt_tix_dup_zeros$obs_id ) ]

dt_tix_dup_zeros <- dt_tix_dup_zeros[
  ,
  if ( any( valor_tarifa < 1e-01 ) & any( valor_tarifa > 0 ) ){
    .SD[ valor_tarifa > 0 ][ 1 ] 
  } else if ( any( valor_total < 1e-01 ) & any( valor_total > 0 ) ){
    .SD[ valor_total > 0 ][ 1 ]
  } else {
    # Code should never get here
    .SD[ 1 ]
  },
  by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ]

dt_tix_dup_zeros
#
# What problems remain? ---------------------------------------------------

dt_tix_duplicates[
  order( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ][ 1:2 ]
# Differences: numero_equipamento_fiscal, numero_bilhete, plataforma_embarque, valor_percentual_desconto, origem_emissao. Irrelevant differences, though.
dt_tix_duplicates[
  order( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ][ 3:4 ]
# Differences: similar case.
dt_tix_duplicates[
  order( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ][ 5:6 ]
# Differences: similar case
dt_tix_duplicates[
  order( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ][ 7:8 ] 
# Differences: similar case.

# # Are there any cases with differences beyond numero_bilhete? --------------
# 
# # Note that obs_id will also differ, so I should take care of that
# tmp <- dt_tix_duplicates[
#   ,
#   { tmp <- vapply( .SD, FUN = function( x ){ length( unique( x ) ) }, numeric( 1 ) )
#   tmp <- tmp[ tmp > 1 ]
#   if ( any( !( tmp %in% c( "numero_bilhete", "obs_id" ) ) ) ){
#     .SD
#   }
#     },
#   by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
# ]
# 
# tmp[ 1:2 ]
# # Numero equipamento fiscal differs. I really don't care.
# # So, within a group, if all relevant variables are the same, return the first observation
# # Relevant variables: tipo_servico, tipo_gratitude, and all "valor" variables
# 
# # First, how big of a chunk will that take care of?
# # This takes a while
# dt_tix_duplicates[
#   ,
#   if ( all( vapply( X = .SD[ , .( tipo_servico, tipo_gratitude, valor_tarifa, valor_percentual_desconto, valor_aliquota_icms, valor_pedagio, valor_taxa_embarque, valor_total ) ],
#                FUN = function( x ){ length( unique( x ) ) },
#                FUN.VALUE = numeric( 1 )
#                ) == 1
#             ) ){
#     .SD
#   },
#   by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
# ] # 75,320 out of 75,495!
# 
# # Return the cases for which this *doesn't* work
# # Tiny change in the case: negate the all() statement
# dt_holdouts <- dt_tix_duplicates[
#   ,
#   if ( !all( vapply( X = .SD[ , .( tipo_servico, tipo_gratitude, valor_tarifa, valor_percentual_desconto, valor_aliquota_icms, valor_pedagio, valor_taxa_embarque, valor_total ) ],
#                     FUN = function( x ){ length( unique( x ) ) },
#                     FUN.VALUE = numeric( 1 )
#   ) == 1
#   ) ){
#     .SD
#   },
#   by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
#   ]
# 
# # Take them out of dt_tix_duplicates
# dt_tix_duplicates <- dt_tix_duplicates[ !( obs_id %in% dt_holdouts$obs_id ) ]

# What's their problems?
dt_tix_duplicates[
  order( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
]
# A few cases with 99.99 discount and 100 discount (as seen above)
# A few cases with one 0 discount and posistive discount where the positive discount is the correct one

# Any cases in which the tipo_gratitude varies?
dt_tix_duplicates[
  ,
  if ( length( unique( tipo_gratitude ) ) > 1 ) .SD,
  by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ] # Nope!

# How many groups do I have?
dt_tix_duplicates[
  ,
  .N,
  by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
] # 86 groups

# How many records per group?
dt_tix_duplicates[
  ,
  .N,
  by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ][
    ,
    unique( N )
  ] # 2, 3, 4

# Look at the 3's and 4's
dt_tix_duplicates[
  ,
  if ( .N > 2 ) .SD,
  by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ]
# 3: Grab the first one
# 4: Grab one of the records with 50%

# If all obs ask for 100% discount, grab one of the observations with 100% discount
# If they ask for 50%, the same
# If they ask for tarifa normal, grab an observation that "matches"
dt_tix_duplicates <- dt_tix_duplicates[
  ,
  if ( all( str_detect( tipo_gratitude, "100" ) ) ){
    .SD[ which.min( valor_percentual_desconto - 100 ) ] # searching for approximately 100 with dplyr::near gives problems
  } else if ( all( str_detect( tipo_gratitude, "50" ) ) ){
    .SD[ which.min( valor_percentual_desconto - 50 ) ] # searching for approximately 50 with near gives problems
  } else if ( all( str_detect( tipo_gratitude, "Normal" ) ) ){
    if ( .SD[ , any( near( valor_tarifa + valor_taxa_embarque + valor_pedagio, valor_total ) ) ] ){
      .SD[ near( valor_tarifa + valor_taxa_embarque + valor_pedagio, valor_total ) ][ 1 ]
    } else if ( .SD[ , any( near( valor_tarifa * ( 1 - valor_percentual_desconto / 100 ) + valor_taxa_embarque + valor_pedagio, valor_total ) ) ] ){
      .SD[ near( valor_tarifa * ( 1 - valor_percentual_desconto / 100 ) + valor_taxa_embarque + valor_pedagio, valor_total ) ][ 1 ]
    } else {
      .SD[ which.max( valor_percentual_desconto ) ] # Had to add this bc I was missing some cases. I'm maxing valor_percentual_desconto bc of examples above
    }
  } else {
    # I don't think the code should get here
    .SD[ which.max( valor_total ) ]
  },
  by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
]

# Finally, put everything together ----------------------------------------

# Check that all names agree
lapply( list( data_tickets, dt_tix_duplicates_irrelevant, dt_tix_duplicates_service, dt_tix_dup_gratitude, dt_tix_dup_zeros, dt_tix_duplicates ),
        FUN = function( x ){ all( names( x ) %in% names( data_tickets ) ) } )

# Bind all datasets
data_tickets <- rbindlist( list( data_tickets, dt_tix_duplicates_irrelevant, dt_tix_duplicates_service, dt_tix_dup_gratitude, dt_tix_dup_zeros, dt_tix_duplicates ),
                           use.names = TRUE )

# Check: are there any problems remaining? --------------------------------

num_probs <- nrow( data_tickets[
  ,
  if ( .N > 1 ) .SD,
  by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
] )

# If so, grab records by some arbitrary rule
if ( num_probs > 0 ){
  data_tickets <- data_tickets[
    ,
    .SD[ 1 ],
    by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
  ]
}

# Save
fwrite( x = data_tickets, file = "output/dt_tix_wout_duplicates_09_2019.csv" )