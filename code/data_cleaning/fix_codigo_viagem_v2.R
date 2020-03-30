# This script fixes the problems in `codigo_viagem`

# This script looks into dynamic price discrimination

# Clean-up, packages, classes and functions -------------------------------

rm( list = ls() )

library( tidyverse )
library( data.table )
library( lubridate )
library( readxl )


# Check files, define dates -------------------------------------------------------------

list.files( "raw_data/monitriip/bilhetes_de_passagem", full.names = TRUE )
list.files( "output/" )
list.files( "raw_data/monitriip/viagens", full.names = TRUE )

dates <- c( paste0( 0, 1:9, "_2019" ),
            paste0( 10:12, "_2019" ),
            "01_2020" )
#

# Load data -----------------------------------------------------------

# >>>>> Price data for September/2019 <<<<<
#data_tickets <- fread( paste0( "raw_data/monitriip/bilhetes_de_passagem/venda_passagem_", dates[[ 9 ]], ".csv" ), integer64 = "character" )
data_tickets <- fread( paste0( "output/dt_tix_wout_duplicates_", dates[[ 9 ]], ".csv" ), integer64 = "character" )
data_tickets
str( data_tickets )

# >>>>> Trip data, September 2019 <<<<<
#data_trips <- fread( paste0( "raw_data/monitriip/viagens/viagem_regular_", dates[[ 1 ]], ".csv" ), integer64 = "character" )
#data_trips
data_trips <- fread( "output/dt_trips_od_no_duplicates.csv" )
data_trips <- data_trips[ lubridate::month( lubridate::dmy( data_viagem_programada ) ) == 9 ]

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

# data_tickets <- unique( data_tickets, by = 2:length( data_tickets ) ) # unique on everything other than codigo_viagem
# #data_trips <- unique( data_trips, by = 2:length( data_trips ) )
# 
# data_tickets[
#   ,
#   if ( .N > 1 ) .SD,
#   #by = .( codigo_viagem, numero_bilhete, data_emissao_bilhete, hora_emissao_bilhete, ponto_origem_viagem, ponto_destino_viagem )
#   by = .( codigo_viagem, numero_bilhete )
# ] # There are lots of 0's in numero_bilhete here, so that's not a great variable.
# 
# data_tickets[ nu_linha == 19004961 ][
#   ,
#   if ( .N > 1 ) .SD,
#   by = .( data_viagem, hora_viagem, ponto_origem_viagem, ponto_destino_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
# ]
# # There seem to be some duplicates due to records with categoria_transporte and tipo_servico being "Nao Se Aplica"
# # I will keep only tickets with categoria_transporte == "Interestadual" below.

# Drop international routes -----------------------------------------------

# data_tickets[ , unique( categoria_transporte ) ]
# data_tickets[ , summary( as.factor( categoria_transporte ) ) ]
# 
# data_tickets <- data_tickets[ categoria_transporte == "Interestadual" ]
# 
# # Check the case above again
# data_tickets[ nu_linha == 19004961 ][
#   ,
#   if ( .N > 1 ) .SD,
#   by = .( data_viagem, hora_viagem, ponto_origem_viagem, ponto_destino_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
#   ] # No more duplicates here.
# 
# # Keeping only categoria_transporte == "Interestadual" doesn't seem to be enough.
# # But every origin, destination comes with a /STATE or /COUNTRY bit
# # Extract that
# data_tickets[ , unique( str_extract( string = ponto_origem_viagem, pattern = "/\\w+" ) ) ]
# # Drop URY, PRY and ARG
# data_tickets <- data_tickets[ !( str_extract( string = ponto_origem_viagem, pattern = "/\\w+" ) %in% c( "/URY", "/PRY", "/ARG" ) ) ]
# # Check
# data_tickets[ , unique( str_extract( string = ponto_origem_viagem, pattern = "/\\w+" ) ) ] # Good
# 
# # Do the same with ponto_destino_viagem
# data_tickets[ , unique( str_extract( string = ponto_destino_viagem, pattern = "/\\w+" ) ) ]
# # Drop PRY, ARG and BOL
# data_tickets <- data_tickets[ !( str_extract( string = ponto_destino_viagem, pattern = "/\\w+" ) %in% c( "/URY", "/PRY", "/ARG", "/BOL" ) ) ]
# # Check
# data_tickets[ , unique( str_extract( string = ponto_destino_viagem, pattern = "/\\w+" ) ) ] # Good
# 
# # Any crazy prices remaining?
# data_tickets[ valor_tarifa > 2000 ] # Nope
# data_tickets[ , max( valor_tarifa ) ] # 1285.09
# # Check that it makes sense
# data_tickets[ valor_tarifa > 1200 ] # reasonable.

# Check remaining duplicates ----------------------------------------------

# data_tickets[
#   ,
#   if ( .N > 1 ) .SD,
#   by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
# ]
# # Different cases:
# # 1-2. Different codigo_viagem, numero_bilhete, tipo_gratitude, valores
# # 3-4. Different codigo_viagem, tipo_servico, 
# # 7-8 (of the 10 that print).

# Sketch of steps to follow -----------------------------------------------

# 1. For trips with *no confounding trips* [meaning that they're the only trip with that nu_linha/data_viagem_programada/hora_viagem_programada]
#    grab the `codigo_viagem` from data_trips.
# 2. For the remaining trips [i.e., there are other trips with the same nu_linha/data_viagem_programada/hora_viagem_programada], 
#    also use `sentido_linha` for the merge. Note that `sentido_linha` has to be constructed for `data_tickets` [done below].
# >>> NOTE <<<
# This *assumes* that there is at most one trip per nu_linha/data/hora/sentido. Is that true?
# In other words, do nu_linha/data_viagem_programada/hora_viagem_programada/sentido_linha uniquely identify a trip in data_trips?

# Unique identification of observations in data_trips ---------------------

data_trips[ , .N, by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha ) ] # 97,372 groups.

# Look only at the groups with more than 1 observation.
data_trips[ , if ( .N > 1 ) .SD, by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha ) ]
# A few records seem to be incorrect. E.g., record 1 [see times]
# 3-4 could be different trips, though they stop in different places.
# 4321-4322 may seem like different trips but they're a key case of duplicates: when there is a `viagem de transbordo`. Keep only one of those.
# 4323 seems to be a mistake: the trip starts way before the scheduled time and lasts 11 seconds.
# Look at the trips in rows 4232-4234
#data_tickets[ codigo_viagem %in% c( 2112317, 2112318 ) ]
# Maybe duplicates [different data_emissao_bilhete/hora_emissao_bilhete]

# Keep only one codigo_viagem for trips within a `viagem de transbordo` group
# First create an observation ID in data_trips to be able to take records out, clean, and add them back
data_trips[ , obs_id := seq_len( .N ) ][]

# Define the records corresponding to a group with "transbordo"
data_trips_transbordo <- data_trips[ ,
                                     if ( any( in_transbordo == "SIM" ) ) .SD,
                                     by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
                                     ]

# Take these records out of data_trips
data_trips <- data_trips[ !( obs_id %in% data_trips_transbordo$obs_id ) ]

# Check groups with more than 2 observations
data_trips_transbordo[ ,
                       if ( .N > 2 ) .SD,
                       by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
                       ] 
# There seems to be a single observation per group with in_transbordo == NAO.
# Keep that observation.
data_trips_transbordo <- data_trips_transbordo[ ,
                                                .SD[ in_transbordo != "SIM" ],
                                                by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
                                                ]

# Check that you get one trip per group
data_trips_transbordo[ ,
                       .N,
                       by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
                       ][ ,
                          unique( N )
                          ] # Check

# Clean remaining duplicates

# Candidate set of variables that uniquely identifies a trip: nu_linha/data/hora/sentido/placa/hora_inicio/hora_fim
# The last two are probably redundant
data_trips_duplicates <- data_trips[ ,
                                     if ( .N > 1 ) .SD,
                                     by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha, placa )
                                     ]
data_trips_duplicates
head( data_trips_duplicates )
tail( data_trips_duplicates )
# All of these seem to be due to tipo_viagem being 0 and something other than zero
# Is that indeed the case?
data_trips_duplicates[ ,
                       .( unique_types = length( unique( tipo_viagem ) ) ),
                       by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha, placa )
                       ]
# Most of them are, but not all. Look into the cases that are not.
data_trips_duplicates[ ,
                       if ( length( unique( tipo_viagem ) ) == 1 ) .SD,
                       by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha, placa )
                       ]
# These really seems like duplicates. Some weird stuff: trips across nu_linha with exactly the same start and end times.
# Just take one observation each. 
# First take the duplicate observations out of data_trips
data_trips <- data_trips[ !( obs_id %in% data_trips_duplicates$obs_id ) ]
# Now keep only one of the records identified above
# First define them.
data_trips_duplicates_other <- data_trips_duplicates[ ,
                                                      if ( length( unique( tipo_viagem ) ) == 1 ) .SD,
                                                      by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha, placa )
                                                      ]
# Then take them out of data_trips_duplicates
data_trips_duplicates <- data_trips_duplicates[ !( obs_id %in% data_trips_duplicates_other$obs_id ) ]
# Then keep only one of them for each group
data_trips_duplicates_other <- data_trips_duplicates_other[ ,
                                                            .SD[ 1 ],
                                                            by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha, placa )
                                                            ]
# Now fix the records that *are* cases of tipo_viagem being zero and something else
# First check that's indeed the case [repetition of code above]
data_trips_duplicates[ ,
                       .( unique_types = length( unique( tipo_viagem ) ) ),
                       by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha, placa )
                       ][ ,
                          unique( unique_types )
                          ] # always 2

# Is it always one zero and one non-zero?
data_trips_duplicates[ ,
                    .( has_zero = 0 %in% tipo_viagem, has_non_zero = any( tipo_viagem != 0 ) ),
                    by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha, placa )
                    ] # Yes for September, but not *always* the case.

# *For these trips* keep only tipo_viagem = 0 [Note this is arbitrary]
# Note that grabbing tipo_viagem = 0 won't always work because in a few cases there's no such trip.
# Grab the first observation of each group
data_trips_duplicates <- data_trips_duplicates[ ,
                                                if ( 0 %in% tipo_viagem ) .SD[ tipo_viagem == 0 ] else .SD[ 1 ],
                                                by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha, placa )
                                                ]

#data_trips_duplicates <- data_trips_duplicates[ tipo_viagem == 0 ]

# Reconstruct data_trips as the rbind of data_trips, data_trips_transbordo, data_trips_duplicates_other and data_trips_duplicates
data_trips <- rbindlist( list( data_trips, data_trips_transbordo, data_trips_duplicates_other, data_trips_duplicates ), use.names = TRUE )

# Check that trips are uniquely identified by nu_linha/data/hora/sentido/placa
data_trips[ ,
            .N,
            by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha, placa )
            ][ , 
               unique( N )
               ] # 1.

# Checking the clean data_trips -------------------------------------------

# Q: How frequent are trips at the same exact time?
data_trips[ , if ( .N > 1 ) .SD, by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha ) ] # 2,023.
# Most of these seem to be mistakes, but some seem to be different trips.

# Cleaning: create a sentido_linha variable in data_tickets ----------------------------------------------------------------

# Separete origin_destination_linha into two variables: `ponto_origem_linha` and `ponto_destino_linha`
data_tickets[ , c( "ponto_origem_linha", "ponto_destino_linha" ) := tstrsplit( origem_destino_linha, split = " - " ) ]
# Note that this introduces NAs. That seems to be due to unrecgonized Portuguese characters.
data_tickets[ is.na( ponto_origem_linha ) | is.na( ponto_destino_linha ) ] # Yep. One international route with an interstate subroute.

# Create estado_origem_linha and estado_destino_linha
data_tickets[ , `:=`( estado_origem_linha = str_extract( ponto_origem_linha, "\\([[:alpha:]]{2}\\)" ),
                      estado_destino_linha = str_extract( ponto_destino_linha, "\\([[:alpha:]]{2}\\)" ) ) ]
# Extract just the letters in the two variables just created
data_tickets[ , `:=`( estado_origem_linha = str_extract( estado_origem_linha, "[[:alpha:]]{2}" ),
                      estado_destino_linha = str_extract( estado_destino_linha, "[[:alpha:]]{2}" ) ) ][]

# Put ponto_origem_viagem and ponto_destino_viagem [format /RJ] in the same format as ponto_origem_linha and ponto_destino_linha [format (RJ)]
# First extract the state information
data_tickets[ ,
              `:=`( estado_origem_viagem = str_extract( ponto_origem_viagem, "[[:alpha:]]{2}$" ),
                    estado_destino_viagem = str_extract( ponto_destino_viagem, "[[:alpha:]]{2}$" ) )
              ][] 

# Next use the state information to rewrite `ponto_origem_viagem` and `ponto_destino_viagem`. This takes a while.
system.time(
  data_tickets[ ,
                `:=`( ponto_origem_viagem = str_replace( ponto_origem_viagem, pattern = "/[[:alpha:]]{2}", replacement = paste0( "(", estado_origem_viagem, ")" ) ),
                      ponto_destino_viagem = str_replace( ponto_destino_viagem, pattern = "/[[:alpha:]]{2}", replacement = paste0( "(", estado_destino_viagem, ")" ) )
                )
                ][] 
) # 73.3 seconds

# Now use these variables to create a sentido_linha variable [See notes `fix_codigo_viagem.txt`]
data_tickets[ ,
              sentido_linha := case_when(
                ponto_origem_viagem == ponto_origem_linha ~ 0, # ida
                ponto_origem_viagem == ponto_destino_linha ~ 1, # volta
                ponto_destino_viagem == ponto_destino_linha ~ 0, # ida
                ponto_destino_viagem == ponto_origem_linha ~ 1, # volta
                # Cases based on the origin state
                #estado_origem_viagem == estado_origem_linha & estado_destino_viagem == estado_destino_linha ~ 0, # ida
                #estado_origem_viagem == estado_destino_linha & estado_destino_viagem == estado_origem_linha ~ 1, # volta
                estado_origem_viagem == estado_origem_linha & estado_destino_viagem != estado_origem_linha ~ 0, # ida
                estado_origem_viagem == estado_destino_linha & estado_destino_viagem != estado_destino_linha ~ 1, # volta
                # Cases based on the destination state
                estado_destino_viagem == estado_origem_linha & estado_origem_viagem != estado_origem_linha ~ 1, # volta
                estado_destino_viagem == estado_destino_linha & estado_origem_viagem != estado_destino_linha ~ 0, # ida
                TRUE ~ NA_real_
              )
              ][ , sum( is.na( sentido_linha ) ) ] # 63,380 NAs

data_tickets[ is.na( sentido_linha ) ]

# Join trip ID for trips with no confounding trips ------------------------

# Here "no confounding trips" means that there are no *other* trips with the same nu_linha/data/hora.

# I wanna join on nu_linha, date and time. Make sure they're the same types and same value format across the two DTs.
str( data_tickets[ , .( nu_linha, data_viagem, hora_viagem ) ] )
str( data_trips[ , .( nu_linha, data_viagem_programada, hora_viagem_programada ) ] )
data_tickets[ , sample( x = data_viagem, size = 20, replace = FALSE ) ] # dmy
data_trips[ , sample( x = data_viagem_programada, size = 20, replace = FALSE ) ] # dmy

# Create an observation id in data_tickets. This will be used to know which observations I've already assigned a codigo_viagem to.
data_tickets[ , `:=` ( obs_id = seq_len( .N ) ) ][]

# Merge only trips with no confounding trips
data_tickets_no_conf <- merge.data.table( x = data_tickets,
                                          y = data_trips[ ,
                                                          if ( .N == 1 ) .SD,
                                                          by = .( nu_linha, data_viagem_programada, hora_viagem_programada )
                                                          ][ ,
                                                             .( codigo_viagem, nu_linha, data_viagem_programada, hora_viagem_programada )
                                                             ],
                                          by.x = c( "nu_linha", "data_viagem", "hora_viagem" ),
                                          by.y = c( "nu_linha", "data_viagem_programada", "hora_viagem_programada" ) )

data_tickets_no_conf[ , all( codigo_viagem.x == codigo_viagem.y ) ] # FALSE.
# The line above returns `TRUE` if run *without* cleaning data_trips.
# What's happening is that now more trips are returned by the condition if ( .N == 1 ) and some of the matched trips won't necessarily match with what ANTT did.
# Note though that for the vast majority of trips, I get the same result
data_tickets_no_conf[ codigo_viagem.x != codigo_viagem.y ] # 24,531 observations.

# Take the matched records out of data_tickets.
data_tickets <- data_tickets[ !( obs_id %in% data_tickets_no_conf$obs_id ) ] # 242,080 observations.
data_tickets

# Merge data_tickets and data_trips on nu_linha/data/hora/sentido
data_tickets_step2 <- merge.data.table( x = data_tickets,
                                        y = data_trips[ ,
                                                        if ( .N == 1 ) .SD,
                                                        by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
                                                        ][ ,
                                                           .( codigo_viagem, nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
                                                           ],
                                        by.x = c( "nu_linha", "data_viagem", "hora_viagem", "sentido_linha" ),
                                        by.y = c( "nu_linha", "data_viagem_programada", "hora_viagem_programada", "sentido_linha" )
)

# Take the matched records out of data_tickets
data_tickets <- data_tickets[ !( obs_id %in% data_tickets_step2$obs_id ) ] # 22,784
data_tickets

# Initialize a column codigo_viagem.y [same name as the one in the datasets above]
data_tickets$codigo_viagem.y <- NA_integer_
data_tickets

# Assign a codigo_viagem to the unassigned records
# This will take a LOOOONG time.
for ( ix in seq_len( nrow( data_tickets ) ) ){
  
  # Look for trips matching nu_linha/data/hora/sentido
  # First define temporary variables holding those values
  tmp_nu_linha <- data_tickets$nu_linha[[ ix ]] # *note* this is about 100 times faster than the dt alternative data_tickets[ ix, nu_linha ]
  tmp_data <- data_tickets$data_viagem[[ ix ]]
  tmp_hora <- data_tickets$hora_viagem[[ ix ]]
  tmp_sentido <- data_tickets$sentido_linha[[ ix ]]
  
  # Now filter data_trips
  matching_records <- data_trips[ nu_linha == tmp_nu_linha & data_viagem_programada == tmp_data & hora_viagem_programada == tmp_hora & sentido_linha == tmp_sentido ]
  
  if ( nrow( matching_records ) >= 1 ){
    # If there is at least one matching record, take the codigo_viagem from the first row
    data_tickets$codigo_viagem.y[[ ix ]] <- matching_records$codigo_viagem[[ 1 ]]
  } else {
    # In which case there are no matching records
    # This occurs, e.g., when tmp_sentido == NA
    # Look for records matching only nu_linha/data/hora
    matching_records <- data_trips[ nu_linha == tmp_nu_linha & data_viagem_programada == tmp_data & hora_viagem_programada == tmp_hora ]
    if ( nrow( matching_records ) >= 1 ){
      # If at least one matching record is found [I think this should always be the case]
      data_tickets$codigo_viagem.y[[ ix ]] <- matching_records$codigo_viagem[[ 1 ]]
    } 
  }
  
  if ( ix %% 1000 == 0 ){
    cat( "Date:", date[[ ix ]], ".Done with iteration ", ix, ".\n" )
  }
  
}

# Does every ticket get a trip?
data_tickets[ , sum( is.na( codigo_viagem.y ) ) ] # Yep =)
