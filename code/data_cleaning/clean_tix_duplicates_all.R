# This script cleans the duplicates in the tickets datasets for all months.

# Clean-up, packages, classes and functions -------------------------------

rm( list = ls() )

library( data.table )
library( tidyverse )
library( lubridate )

# Check files, define dates -------------------------------------------------------------

list.files( "raw_data/monitriip/bilhetes_de_passagem", full.names = TRUE )
list.files( "raw_data/monitriip/viagens", full.names = TRUE )

dates <- c( paste0( 0, 1:9, "_2019" ),
            paste0( 10:12, "_2019" ),
            paste0( 0, 1, "_2020" ) )

# Main loop ---------------------------------------------------------------

for ( ix in ( length( dates ) - 1L ):length( dates ) ){
  
  cat( ">>>>> Cleaning ", dates[[ ix ]], ". <<<<< \n", sep = "" )
  
  # Load the data
  # If the date is at least Dec 2019, load one of the _26_fields files. Otherwise, run the raw file.
  current_date <- lubridate::dmy( paste0( "01_", dates[[ ix ]] ) )
  if ( current_date >= "2019-12-01" ){
    data_tickets <- fread( paste0( "raw_data/monitriip/bilhetes_de_passagem/venda_passagem_", dates[[ ix ]], "_26_fields.csv" ) )
  } else {
    data_tickets <- fread( paste0( "raw_data/monitriip/bilhetes_de_passagem/venda_passagem_", dates[[ ix ]], ".csv" ),
                           integer64 = "character" )  
  }
  
  # Unique on everything other than codigo_viagem
  data_tickets <- unique( data_tickets, by = 2:length( data_tickets ) )
  
  # Drop servico semiurbano
  data_tickets <- data_tickets[ tipo_servico != "Semiurbano" ]
  
  # >>>>> Drop international routes <<<<<
  # First keep only categoria_transporte == "Interestadual"
  data_tickets <- data_tickets[ categoria_transporte == "Interestadual" ]
  
  # Drop international trips. They always come with 4 characters in the end, e.g., /URY.
  # Local trips instead come with 3 chars, e.g., /RJ. Use that fact.
  data_tickets <- data_tickets[ nchar( str_extract( string = ponto_origem_viagem, pattern = "/\\w+" ) ) == 3 ]
  
  # Do the same with ponto_destino_viagem
  data_tickets <- data_tickets[ nchar( str_extract( string = ponto_destino_viagem, pattern = "/\\w+" ) ) == 3L ]
  # >>>>> DONE <<<<<
  
  # Create a service index 
  data_tickets[
    ,
    servico_idx := case_when(
      str_detect( tipo_servico, "Convencional sem" ) ~ 0L,
      str_detect( tipo_servico, "Convencional com" ) ~ 1L,
      str_detect( tipo_servico, "Executivo" ) ~ 2L,
      str_detect( tipo_servico, "Semileito" ) ~ 3L,
      str_detect( tipo_servico, "Leito sem" ) ~ 4L,
      str_detect( tipo_servico, "Leito com" ) ~ 5L,
      TRUE ~ NA_integer_
    )
    ]
  
  # Create obs_id
  data_tickets[ , obs_id := seq_len( .N ) ]
  
  # Define dt for duplicates
  dt_tix_duplicates <- data_tickets[
    ,
    if ( .N > 1 ) .SD,
    by = .( cnpj, origem_destino_linha, data_viagem, hora_viagem, ponto_origem_viagem, ponto_destino_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
    ]
  
  # Take duplicate records out of data_tickets
  data_tickets <- data_tickets[ !( obs_id %in% dt_tix_duplicates$obs_id ) ]
  
  cat( dates[[ ix ]], ". Number of duplicates: ", nrow( dt_tix_duplicates ), ".\n", sep = "" )
  
  # >>>>> Clean irrelevant duplicates <<<<<
  
  # Define a dt to keep irrelevant duplicates.
  # This takes a while.
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
  
  # Take these records out of dt_tix_duplicates
  dt_tix_duplicates <- dt_tix_duplicates[ !( obs_id %in% dt_tix_duplicates_irrelevant$obs_id ) ]
  
  # Keep only the first record per group in dt_tix_duplicates_irrelevant
  dt_tix_duplicates_irrelevant <- dt_tix_duplicates_irrelevant[
    ,
    .SD[ 1 ],
    by = .( cnpj, origem_destino_linha, data_viagem, hora_viagem, ponto_origem_viagem, ponto_destino_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
    ]
  
  cat( dates[[ ix ]], ". Cleaned irrelevant duplicates. Duplicates remaining: ", nrow( dt_tix_duplicates ), ".\n", sep = "" )
  
  # >>>>> DONE <<<<<
  
  # >>>>> Clean duplicates with multiple tipo_servico <<<<<
  
  # Define a dt for records with multiple tipo_servico
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

  cat( dates[[ ix ]], ". Cleaned records with multiple tipo_servico. Duplicates remaining: ", nrow( dt_tix_duplicates ), ".\n", sep = "" )
  
  # >>>>> DONE <<<<<
    
  # >>>>> Fix duplicates with different tipo_gratitude <<<<<
  
  # Define a dataset for those records
  dt_tix_dup_gratitude <- dt_tix_duplicates[
    ,
    if ( length( unique( tipo_gratitude ) ) > 1 ) .SD,
    by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
    ] # 10,829
  
  # Take them out of dt_tix_duplicates
  dt_tix_duplicates <- dt_tix_duplicates[ !( obs_id %in% dt_tix_dup_gratitude$obs_id ) ]
  
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
  
  cat( dates[[ ix ]], ". Cleaned records with multiple tipo_gratitude. Duplicates remaining: ", nrow( dt_tix_duplicates ), ".\n", sep = "" )
  
  # >>>>> DONE <<<<<
  
  # >>>>> Deal with the cases with zeros <<<<<
  
  # save these cases in a dt
  dt_tix_dup_zeros <- dt_tix_duplicates[
    ,
    if ( ( any( valor_tarifa < 1e-01 ) & any( valor_tarifa > 0 ) ) | ( any( valor_total < 1e-01 ) & any( valor_total > 0 ) ) ) .SD,
    by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
    ]
  
  # Take records out of dt_tix_duplicates
  dt_tix_duplicates <- dt_tix_duplicates[ !( obs_id %in% dt_tix_dup_zeros$obs_id ) ]
  
  # I'll again choose to keep the positive one.
  # These 100% discoutns might be miscoded discounts, but to think of the pricing behavior, I guess it's better to keep the "real" price
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
  
  cat( dates[[ ix ]], ". Cleaned records with zeros and a non-zero. Duplicates remaining: ", nrow( dt_tix_duplicates ), ".\n", sep = "" )
  
  # >>>>> DONE <<<<<

  # >>>>> Clean remaining cases <<<<<
  
  cat( dates[[ ix ]], ". Cleaning remaining duplicates based on tipo_gratitude.\n", sep = "" )
  
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
  
  # >>>> Finally, put everything together <<<<<
  
  cat( dates[[ ix ]], ". Bind dts.\n", sep = "" )
  
  data_tickets <- rbindlist( list( data_tickets, dt_tix_duplicates_irrelevant, dt_tix_duplicates_service, dt_tix_dup_gratitude, dt_tix_dup_zeros, dt_tix_duplicates ),
                             use.names = TRUE )
  
  # >>>>> Fix any remaining problems <<<<<
  
  cat( dates[[ ix ]], ". Fix any remaining problems.\n", sep = "" )
  
  num_probs <- nrow( data_tickets[
    ,
    if ( .N > 1 ) .SD,
    by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
    ] )
  
  cat( dates[[ ix ]], ". Remaining problems: ", num_probs, ".\n", sep = "" )
  
  # If so, grab records by some arbitrary rule
  if ( num_probs > 0 ){
    data_tickets <- data_tickets[
      ,
      .SD[ 1 ],
      by = .( cnpj, origem_destino_linha, ponto_origem_viagem, ponto_destino_viagem, data_viagem, hora_viagem, data_emissao_bilhete, hora_emissao_bilhete, numero_poltrona )
      ]
  }
  
  # >>>>> DONE <<<<<
  
  # >>>>> Delete columns introduced above <<<<<
  data_tickets[
    ,
    `:=`(
      servico_idx = NULL,
      obs_id = NULL
    )
  ]
  
  # >>>>> Save <<<<<<
  cat( dates[[ ix ]], ". Save.\n", sep = "" )
  # Remove the file in the `output` folder if it exists.
  if ( file.exists( paste0( "output/dt_tix_wout_duplicates_", dates[[ ix ]], ".csv" ) ) ){
    file.remove( paste0( "output/dt_tix_wout_duplicates_", dates[[ ix ]], ".csv" ) )
  }
  # Save
  fwrite( x = data_tickets, file = paste0( "clean_data/dt_tix_wout_duplicates_", dates[[ ix ]], ".csv" ) )
  
}
