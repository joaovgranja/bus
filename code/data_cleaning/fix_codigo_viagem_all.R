# This script essentially wraps the code from fix_codigo_viagem_v2.R into a loop fixing all dates and saving the result.
# There a few small changes though.
# Check that script for more comments and code printing output to the console [for development purposes].
  # That code also shows examples of what problems are being fixed.

# Clean-up, packages, classes and functions -------------------------------

rm( list = ls() )

library( tidyverse )
library( data.table )
library( lubridate )
library( readxl )

# Check files, define dates -------------------------------------------------------------

dates <- c( paste0( 0, 1:9, "_2019" ),
            paste0( 10:11, "_2019" ) )

for ( ix in seq_along( dates ) ){

# >>>>> Load the data <<<<<  
  
# >>>>> Price data for September/2019 <<<<<
#data_tickets <- fread( paste0( "raw_data/monitriip/bilhetes_de_passagem/venda_passagem_", dates[[ ix ]], ".csv" ), integer64 = "character" )
data_tickets <- fread( paste0( "output/dt_tix_wout_duplicates_", dates[[ ix ]], ".csv" ), integer64 = "character" )  
# Get rid of duplicates
#data_tickets <- unique( data_tickets, by = 2:length( data_tickets ) ) # unique on everything other than codigo_viagem

# >>>>> Trip data, September 2019 <<<<<
#data_trips <- fread( paste0( "raw_data/monitriip/viagens/viagem_regular_", dates[[ ix ]], ".csv" ), integer64 = "character" )
#data_trips <- unique( data_trips, by = 2:length( data_trips ) )
data_trips <- fread( "output/dt_trips_od_no_duplicates.csv" )
data_trips <- data_trips[ lubridate::month( lubridate::dmy( data_viagem_programada ) ) == ix ]

# >>>>> Drop international routes <<<<<

# Keep only "Interestadual" routes.
#data_tickets <- data_tickets[ categoria_transporte == "Interestadual" ]

# Drop international trips. They always come with 4 characters in the end, e.g., /URY.
# Local trips instead come with 3 chars, e.g., /RJ. Use that fact.
#data_tickets <- data_tickets[ nchar( str_extract( string = ponto_origem_viagem, pattern = "/\\w+" ) ) == 3 ]

# Do the same with ponto_destino_viagem
#ata_tickets <- data_tickets[ nchar( str_extract( string = ponto_destino_viagem, pattern = "/\\w+" ) ) == 3L ]

# Goal: Fix `codigo_viagem` in the tickets dataset, so we know what product consumers are buying.
# See `fix_codigo_viagem_v2.R` for more detailed description of the steps below.

# >>>>> Unique identification of observations in data_trips <<<<<

# Keep only one codigo_viagem for trips within a `viagem de transbordo` group
# First create an observation ID in data_trips to be able to take records out, clean, and add them back
#data_trips[ , obs_id := seq_len( .N ) ]

# Define the records corresponding to a group with "transbordo"
# data_trips_transbordo <- data_trips[ ,
#                                      if ( any( in_transbordo == "SIM" ) ) .SD,
#                                      by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
#                                      ]

# Take these records out of data_trips
#data_trips <- data_trips[ !( obs_id %in% data_trips_transbordo$obs_id ) ]

# There is a single observation per group with in_transbordo == NAO.
# Keep that observation.
# data_trips_transbordo <- data_trips_transbordo[ ,
#                                                 .SD[ in_transbordo != "SIM" ],
#                                                 by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
#                                                 ]

# Clean remaining duplicates
# Either multiple `tipo_viagem` or what seem to be data errors.
# data_trips_duplicates <- data_trips[ ,
#                                      if ( .N > 1 ) .SD,
#                                      by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha, placa )
#                                      ]

# First take the duplicate observations out of data_trips
#data_trips <- data_trips[ !( obs_id %in% data_trips_duplicates$obs_id ) ]
# First deal with the "unexplained" duplicates [i.e., it's not the case that they have multiple "tipo_viagem"]
# data_trips_duplicates_other <- data_trips_duplicates[ ,
#                                                       if ( length( unique( tipo_viagem ) ) == 1 ) .SD,
#                                                       by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha, placa )
#                                                       ]
# Then take them out of data_trips_duplicates
#data_trips_duplicates <- data_trips_duplicates[ !( obs_id %in% data_trips_duplicates_other$obs_id ) ]
# Then keep only one of them for each group
# data_trips_duplicates_other <- data_trips_duplicates_other[ ,
#                                                             .SD[ 1 ],
#                                                             by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha, placa )
#                                                             ]
# Now fix the records with more than one `tipo_viagem`
# If there's an observation with tipo_viagem == 0, keep that one, otherwise just grab the first observation.
# data_trips_duplicates <- data_trips_duplicates[ ,
#                                                 if ( 0 %in% tipo_viagem ) .SD[ tipo_viagem == 0 ] else .SD[ 1 ],
#                                                 by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha, placa )
#                                                 ]

# Reconstruct data_trips as the rbind of data_trips, data_trips_transbordo, data_trips_duplicates_other and data_trips_duplicates
#data_trips <- rbindlist( list( data_trips, data_trips_transbordo, data_trips_duplicates_other, data_trips_duplicates ), use.names = TRUE )

# Check that trips are uniquely identified by nu_linha/data/hora/sentido/placa
# cat( "Date: ", dates[[ ix ]], ". Are trips uniquely identified by linha/data/hora/sentido/placa? ",
# data_trips[ ,
#             .N,
#             by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha, placa )
#             ][ ,
#                unique( N )
#                ] == 1, ".\n", sep = "" )

# Cleaning: create a sentido_linha variable in data_tickets ----------------------------------------------------------------

# Separete origin_destination_linha into two variables: `ponto_origem_linha` and `ponto_destino_linha`
data_tickets[ , c( "ponto_origem_linha", "ponto_destino_linha" ) := tstrsplit( origem_destino_linha, split = " - " ) ]
# Note that this introduces NAs. That seems to be due to unrecgonized Portuguese characters.
#data_tickets[ is.na( ponto_origem_linha ) | is.na( ponto_destino_linha ) ] # Yep. One international route with an interstate subroute.

# Create estado_origem_linha and estado_destino_linha
data_tickets[ , `:=`( estado_origem_linha = str_extract( ponto_origem_linha, "\\([[:alpha:]]{2}\\)" ),
                      estado_destino_linha = str_extract( ponto_destino_linha, "\\([[:alpha:]]{2}\\)" ) ) ]
# Extract just the letters in the two variables just created
data_tickets[ , `:=`( estado_origem_linha = str_extract( estado_origem_linha, "[[:alpha:]]{2}" ),
                      estado_destino_linha = str_extract( estado_destino_linha, "[[:alpha:]]{2}" ) ) ]

# Put ponto_origem_viagem and ponto_destino_viagem [format /RJ] in the same format as ponto_origem_linha and ponto_destino_linha [format (RJ)]
# First extract the state information
data_tickets[ ,
              `:=`( estado_origem_viagem = str_extract( ponto_origem_viagem, "[[:alpha:]]{2}$" ),
                    estado_destino_viagem = str_extract( ponto_destino_viagem, "[[:alpha:]]{2}$" ) )
              ] 

# Next use the state information to rewrite `ponto_origem_viagem` and `ponto_destino_viagem`. This takes a while.
data_tickets[ ,
              `:=`( ponto_origem_viagem = str_replace( ponto_origem_viagem, pattern = "/[[:alpha:]]{2}", replacement = paste0( "(", estado_origem_viagem, ")" ) ),
                    ponto_destino_viagem = str_replace( ponto_destino_viagem, pattern = "/[[:alpha:]]{2}", replacement = paste0( "(", estado_destino_viagem, ")" ) )
              )
              ] 

# Now use these variables to create a sentido_linha variable [See notes `fix_codigo_viagem.txt`]
data_tickets[ ,
              sentido_linha := case_when(
                ponto_origem_viagem == ponto_origem_linha ~ 1, # ida
                ponto_origem_viagem == ponto_destino_linha ~ 0, # volta
                ponto_destino_viagem == ponto_destino_linha ~ 1, # ida
                ponto_destino_viagem == ponto_origem_linha ~ 0, # volta
                # Cases based on the origin state
                #estado_origem_viagem == estado_origem_linha & estado_destino_viagem == estado_destino_linha ~ 0, # ida
                #estado_origem_viagem == estado_destino_linha & estado_destino_viagem == estado_origem_linha ~ 1, # volta
                estado_origem_viagem == estado_origem_linha & estado_destino_viagem != estado_origem_linha ~ 1, # ida
                estado_origem_viagem == estado_destino_linha & estado_destino_viagem != estado_destino_linha ~ 0, # volta
                # Cases based on the destination state
                estado_destino_viagem == estado_origem_linha & estado_origem_viagem != estado_origem_linha ~ 0, # volta
                estado_destino_viagem == estado_destino_linha & estado_origem_viagem != estado_destino_linha ~ 1, # ida
                TRUE ~ NA_real_
              )
              ]

# Join trip ID for trips with no confounding trips ------------------------

# Here "no confounding trips" means that there are no *other* trips with the same nu_linha/data/hora.

# Create an observation id in data_tickets. This will be used to know which observations I've already assigned a codigo_viagem to.
data_tickets[ , `:=` ( obs_id = seq_len( .N ) ) ]

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

# Take the matched records out of data_tickets.
data_tickets <- data_tickets[ !( obs_id %in% data_tickets_no_conf$obs_id ) ] # 242,080 observations.

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

# Initialize a column codigo_viagem.y [same name as the one in the datasets above]
# Rename codigo_viagem to codigo_viagem.x
data_tickets[ ,
              `:=`( codigo_viagem.x = codigo_viagem,
                    codigo_viagem.y = NA_integer_,
                    codigo_viagem = NULL )
              ]
#data_tickets$codigo_viagem.y <- NA_integer_

# Assign a codigo_viagem to the unassigned records
# This will take a LOOOONG time.
nr_dt <- nrow( data_tickets )

for ( jx in seq_len( nr_dt ) ){
  
  # Look for trips matching nu_linha/data/hora/sentido
  # First define temporary variables holding those values
  tmp_nu_linha <- data_tickets$nu_linha[[ jx ]] # *note* this is about 100 times faster than the dt alternative data_tickets[ ix, nu_linha ]
  tmp_data <- data_tickets$data_viagem[[ jx ]]
  tmp_hora <- data_tickets$hora_viagem[[ jx ]]
  tmp_sentido <- data_tickets$sentido_linha[[ jx ]]
  
  # Now filter data_trips
  matching_records <- data_trips[ nu_linha == tmp_nu_linha & data_viagem_programada == tmp_data & hora_viagem_programada == tmp_hora & sentido_linha == tmp_sentido ]
  
  if ( nrow( matching_records ) >= 1 ){
    # If there is at least one matching record, take the codigo_viagem from the first row
    data_tickets$codigo_viagem.y[[ jx ]] <- matching_records$codigo_viagem[[ 1 ]]
  } else {
    # In which case there are no matching records
    # This occurs, e.g., when tmp_sentido == NA
    # Look for records matching only nu_linha/data/hora
    matching_records <- data_trips[ nu_linha == tmp_nu_linha & data_viagem_programada == tmp_data & hora_viagem_programada == tmp_hora ]
    if ( nrow( matching_records ) >= 1 ){
      # If at least one matching record is found [I think this should always be the case]
      data_tickets$codigo_viagem.y[[ jx ]] <- matching_records$codigo_viagem[[ 1 ]]
    } 
  }
  
  if ( jx %% 5000 == 0 ){
    cat( "Date: ", dates[[ ix ]], ". Done with iteration ", jx, " out of ", nr_dt, ".\n", sep = "" )
  }
  
}

# Does every ticket get a trip?
cat( "Done with assignment of 'codigo_viagem'. Does every ticket get a trip? ", data_tickets[ , !any( is.na( codigo_viagem.y ) ) ], ".\n" )

# Reconstruct data_tickets
data_tickets <- rbindlist( list( data_tickets_no_conf, data_tickets_step2, data_tickets ), use.names = TRUE )

# Save the result
cat( "Save the results.\n" )
#saveRDS( object = data_tickets, file = paste0( "clean_data/tickets/data_tickets_clean_", dates[[ ix ]], ".Rds" ) )
fwrite( x = data_tickets, file = paste0( "clean_data/tickets/data_tickets_clean_", dates[[ ix ]], ".csv" ) )
#saveRDS( object = data_trips, file = paste0( "clean_data/trips/data_trips_clean_", dates[[ ix ]], ".Rds" ) )
#fwrite( x = data_trips, file = paste0( "clean_data/trips/data_trips_clean_", dates[[ ix ]], ".csv" ) )

}