# Cleaning the trips dataset

# Clean-up, packages, classes and functions -------------------------------

rm( list = ls() )

library( data.table )

# Note that the problem exists in the clean version of trips --------------

# The data used in this chunk was constructed in fix_codigo_viagem_all.R

# clean_trip_files <- list.files( "clean_data/trips", full.names = TRUE )
# list_dt_trips_clean <- lapply( X = clean_trip_files, FUN = fread, integer64 = "character" )
# dt_trips_clean <- rbindlist( list_dt_trips_clean )
# dt_trips_clean[
#   nu_linha %in% c( 1000031, 1000041 )
# ]

# Load data and take unique ---------------------------------------------------------------

dt_trips <- fread( "output/dt_trips_with_origin_dest.csv", integer64 = "character" )
dt_trips

dt_trips <- unique( dt_trips,
                by = seq_along( dt_trips )[ -2 ] # everything other than codigo_viagem
                )

# Examples of duplication --------------------------------------------------

# Example of apparent duplicates
dt_trips[
  nu_linha %in% c( 1000031, 1000041 )
  ][
    order( data_viagem_programada, hora_viagem_programada )
  ]
# Many trips seem to be the same
# Same firm, plate, date, time, start time, end time, direction, (exact same) latitude, longitude, imei, origin, destination
dt_trips[
  ,
  if ( .N > 1 ) .SD,
  by = .( cnpj, placa, tipo_viagem, data_viagem_programada, hora_viagem_programada,
          data_inicio_viagem, data_fim_viagem, sentido_linha, latitude, longitude,
          pdop, numero_imei, in_transbordo, codigo_viagem_transbordo, ponto_origem_linha, ponto_destino_linha )
] # 296,739
# This shows that there are lots of duplicate *nu_linha's*.
# Because the nu_linha information in data_tickets seems to be trustworthy, I will keep these duplicates here.

# Add nu_linha to the grouping variables above
dt_trips[
  ,
  if ( .N > 1 ) .SD,
  by = .( nu_linha, cnpj, placa, tipo_viagem, data_viagem_programada, hora_viagem_programada,
          data_inicio_viagem, data_fim_viagem, sentido_linha, latitude, longitude,
          pdop, numero_imei, in_transbordo, codigo_viagem_transbordo, ponto_origem_linha, ponto_destino_linha )
  ]
#

# nu_linha, data, hora, sentido_linha, placa
dt_trips[
  ,
  if ( .N > 1 ) .SD,
  by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha, placa )
  ] # 4893

# 
dt_trips[
  ,
  if ( .N > 1 ) .SD,
  by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
  ] # 62,875

# Decisions ---------------------------------------------------------------

# I will force uniqueness at the nu_linha, placa, data_viagem_programada, hora_viagem_programada, sentido_linha level.
# Note that some of the remaining duplicates at the nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha 
# may also be incorrect.

# Define an observation ID ------------------------------------------------

dt_trips[ , obs_id := seq_len( .N ) ]

# Start by getting rid of gross duplicates --------------------------------------------------------

# Lots of duplicates! Assign the duplicates to a new dt

# # Now assign duplicates to a new dt
# dt_duplicate_trips <- dt_trips[
#   ,
#   if ( .N > 1 ) .SD,
#   by = .( cnpj, placa, tipo_viagem, data_viagem_programada, hora_viagem_programada,
#           data_inicio_viagem, data_fim_viagem, sentido_linha, latitude, longitude,
#           pdop, numero_imei, in_transbordo, codigo_viagem_transbordo, ponto_origem_linha, ponto_destino_linha )
#   ]
# 
# # Take these records out of dt_trips
# dt_trips <- dt_trips[ !( obs_id %in% dt_duplicate_trips$obs_id ) ]
# 
# # Retain a single route per group and in such a way that whenever a given group occurs you get the same nu_linha
# # Easy way to accomplish that: group, order by nu_linha and always keep the first one.
# dt_duplicate_trips <- dt_duplicate_trips[
#   order( nu_linha ),
#   .SD[ 1 ],
#   by = .( cnpj, placa, tipo_viagem, data_viagem_programada, hora_viagem_programada,
#           data_inicio_viagem, data_fim_viagem, sentido_linha, latitude, longitude,
#           pdop, numero_imei, in_transbordo, codigo_viagem_transbordo, ponto_origem_linha, ponto_destino_linha )
#   ]
# 
# dt_duplicate_trips
# 
# # Is it the case that every nu_linha has a unique origin/destination or does this introduce duplicates?
# dt_duplicate_trips[
#   ,
#   .( count_unique_od = length( unique( paste0( ponto_origem_linha, ponto_destino_linha, "-" ) ) ) ),
#   by = .( nu_linha )
#   ][
#     count_unique_od > 1
#     ] # 0 obs, great!
# 
# # Add these guys back to dt_trips
# dt_trips <- rbindlist( list( dt_trips, dt_duplicate_trips ), use.names = TRUE )
# rm( dt_duplicate_trips )
#
# Inspect the result. Any apparent problems remain? -----------------------

# Start looking into nu_linha/data/hora/sentido
# Note that these need not necessarily identify a single trip, bc there might be multiple
# buses going in the same direction at the same time.
# Nevertheless, it's still useful to see if there are any clear problems.
# dt_trips[
#   ,
#   if ( .N > 1 ) .SD,
#   by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
# ][
#   order( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
# ] # 56,334

# A few of these seem to be due to incorrect sentido_linha (see sentido_linha, latitude and longitude)
# Others are due to transbordo.

# Clean transbordo --------------------------------------------------------

# Some of these remaining problems are due to "transbordo"
dt_trips[
  ,
  if ( any( in_transbordo == "SIM" ) ) .SD,
  by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
  ] # 25,685 observations

# Store them in their own dt
dt_trips_transbordo <- dt_trips[
  ,
  if ( any( in_transbordo == "SIM" ) ) .SD,
  by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
  ]

# Take these records out of data_trips
dt_trips <- dt_trips[ !( obs_id %in% dt_trips_transbordo$obs_id ) ]

# Note that most of the time there's a single observation per group with
# in_transbordo == NAO and codigo_viagem_transbordo == NA
# Not always though
dt_trips_transbordo[
  ,
  sum( in_transbordo != "SIM" ),
  by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
  ][
    ,
    unique( V1 )
    ]

# Check the cases when there's more then 1 trip with in_transbordo == NAO
dt_trips_transbordo[
  ,
  if( sum( in_transbordo != "SIM" ) > 1 ) .SD,
  by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
  ] # Only 51 obs.

# These groups seem to have tipo_viagem both 0 and 1. Check.
dt_trips_transbordo[
  ,
  if( sum( in_transbordo != "SIM" ) > 1 ) .SD,
  by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
  ][
    ,
    if ( length( unique( tipo_viagem ) ) > 1 ) .SD,
    by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
  ] # 48 obs

# Check the obs for which this doesn't hold
dt_trips_transbordo[
  ,
  if( sum( in_transbordo != "SIM" ) > 1 ) .SD,
  by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
  ][
    ,
    if ( length( unique( tipo_viagem ) ) == 1 ) .SD,
    by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
    ] 
# Take care of this case by grabbing the last observation.

# So:
# 1.Keep the only observation with in_transbordo == NAO when it exists
# 2.If it is not unique, keep the observation with in_transbordo == NAO and tipo_viagem == 0
# 3.If that is not unique, keep the last such observation
dt_trips_transbordo <- dt_trips_transbordo[
  ,
  if ( sum( in_transbordo != "SIM" ) == 1 ){
    .SD[ in_transbordo != "SIM" ] 
  } else if ( sum( in_transbordo != "SIM" & tipo_viagem == 0 ) == 1 ){
    .SD[ in_transbordo != "SIM" & tipo_viagem == 0 ]
  } else if ( any( in_transbordo != "SIM" ) ) {
    .SD[ in_transbordo != "SIM" ][ .N ]
  } else {
    .SD[ .N ]
  },
  by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
  ]

# Assign these back to dt_trips
dt_trips <- rbindlist( list( dt_trips, dt_trips_transbordo ), use.names = TRUE )
rm( dt_trips_transbordo )
#
# Clean tipo_viagem == 0 and > 0 --------------------------------------------

dt_trips[
  ,
  if ( length( unique( tipo_viagem ) ) > 1 ) .SD,
  by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
] # 3,868

# Many of these really don't make a lot of sense, so I'll just keep the with a zero
# Are there any groups with no zero?
dt_trips[
  ,
  if ( length( unique( tipo_viagem ) ) > 1 ) .SD,
  by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
  ][
    ,
    if ( !( 0 %in% tipo_viagem ) ) .SD,
    by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
  ] # 8 obs (note that most of them have the same plate too)

# Get the observation with the smallest tipo_viagem
# Define a dt for these trips
dt_trips_mult_types <- dt_trips[
  ,
  if ( length( unique( tipo_viagem ) ) > 1 ) .SD,
  by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
  ]
# Take them out of dt_trips
dt_trips <- dt_trips[ !( obs_id %in% dt_trips_mult_types$obs_id ) ]

# Clean dt_trips_mult_types
dt_trips_mult_types <- dt_trips_mult_types[
  ,
  .SD[ which.min( tipo_viagem ) ],
  by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
]

# Append it back to dt_trips
dt_trips <- rbindlist( list( dt_trips, dt_trips_mult_types ), use.names = TRUE )
rm( dt_trips_mult_types )

# Any problems remaining? -------------------------------------------------

dt_trips[
  ,
  if ( .N > 1 ) .SD,
  by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
] # 32,884

# Now some of these could be genuine trips.
# However, I *will* force uniqueness on linha/data/hora/sentido/placa
dt_trips[
  ,
  if ( .N > 1 ) .SD,
  by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha, placa )
  ] # 282 obs

# Just keep an arbitrary observation
# First define those records
dt_trips_duplicates <- dt_trips[
  ,
  if ( .N > 1 ) .SD,
  by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha, placa )
  ]
# Take them out of dt_trips
dt_trips <- dt_trips[ !( obs_id %in% dt_trips_duplicates$obs_id ) ]
# Clean
dt_trips_duplicates <- dt_trips_duplicates[
  ,
  .SD[ 1 ],
  by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha, placa )
  ]
# Append them back
dt_trips <- rbindlist( list( dt_trips, dt_trips_duplicates ), use.names = TRUE )
rm( dt_trips_duplicates )

# Inspect the linha/data/hora/sentido duplicates one last time ------------

dt_trips[
  ,
  if ( .N > 1 ) .SD,
  by = .( nu_linha, data_viagem_programada, hora_viagem_programada, sentido_linha )
]
# These are all kind of weird (see start and end times, latitude and longitude)
# In the first two there's a mistake in the direction (Google locations and their coordinates)
# In the last ones as well! The -20/-54 are Campo Grande; the -23/-46 are Santos
# Next: fix these records based on the location of the arrival location
# Drop records that don't correspond to any of the two endpoints
# Recode sentido_linha using the endpoint

# Does the rest of the data look correct?
dt_trips[ sample( x = seq_len( nrow( dt_trips ) ), size = 10 ) ]
# Check these cases and Google the coordinates of these localities
# It seems that the coordinates in the data are from the *starting* point,
# not the *endpoint* as the documentation says.
# ---> It's the documentation's explanation of the sentido_linha variable that is incorrect. See the Monitriip resolution.

# Save the result ---------------------------------------------------------

fwrite( x = dt_trips, file = "output/dt_trips_od_no_duplicates.csv" )

# Check that all dates are covered
# dates <- lubridate::dmy( dt_trips$data_viagem_programada )
# range( dates )
