# This script adds route origin and destination information to the trips dataset


# Clean-up, packages, classes and functions -------------------------------

rm( list = ls() )

library( data.table )
library( stringr )

# Load the data -----------------------------------------------------------

list.files( "raw_data/monitriip/viagens", full.names = TRUE )
list.files( "raw_data/monitriip/bilhetes_de_passagem", full.names = TRUE )

# Load all the trip datasets
files_data_trips <- list.files( "raw_data/monitriip/viagens", full.names = TRUE )
files_data_trips <- files_data_trips[ str_detect( files_data_trips, ".csv$" ) ]
list_data_trips <- lapply( X = files_data_trips, FUN = fread, integer64 = "character" )
data_trips <- rbindlist( list_data_trips )
rm( list_data_trips )
data_trips

# Load all of the tickets dataset, but only the route and the route origin-dest info
files_data_tix <- list.files( "raw_data/monitriip/bilhetes_de_passagem", full.names = TRUE )
files_data_tix
# I want to use the _26_fields files
# First keep only 01_2019-11_2019.
files_data_tix <- files_data_tix[ str_detect( files_data_tix, "venda_passagem_\\d{2}_2019.csv" ) ]
files_data_tix <- files_data_tix[ -length( files_data_tix ) ]
# Now append the two _26_fields_files
files_data_tix <- c( files_data_tix,
                     "raw_data/monitriip/bilhetes_de_passagem/venda_passagem_12_2019_26_fields.csv",
                     "raw_data/monitriip/bilhetes_de_passagem/venda_passagem_01_2020_26_fields.csv" )

# Load one file just to check variable names
tmp_tix <- fread( files_data_tix[[ 1 ]], nrows = 10 )
names( tmp_tix )
rm( tmp_tix )
list_data_tix <- lapply( files_data_tix,
                         fread,
                         select = c( "cnpj", "nu_linha", "origem_destino_linha" ) )
str( list_data_tix, 1 ) 
data_tix <- rbindlist( list_data_tix )
rm( list_data_tix )
data_tix
data_tix[ , c( "ponto_origem_linha", "ponto_destino_linha" ) := tstrsplit( origem_destino_linha, split = " - " ) ][] # This takes a while
data_tix[ , origem_destino_linha := NULL ]
data_tix <- unique( data_tix )
data_tix # 2,811 unique lines

# Save the data with routes and origin/dest -------------------------------

fwrite( x = data_tix, file = "clean_data/dt_route_origin_dest.csv" )

# Join this data to data_trips --------------------------------------------

data_trips
data_tix

data_trips <- merge.data.table( x = data_trips,
                                y = data_tix,
                                by = "nu_linha",
                                all.x = TRUE )

# Save data_trips with origin/dest ----------------------------------------

fwrite( x = data_trips, file = "clean_data/dt_trips_with_origin_dest.csv" )




