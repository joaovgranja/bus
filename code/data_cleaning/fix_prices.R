# Clean ticket data
# >>> NOTE: this script starts from cleaned csv files obtained by running fix_codigo_viagem_all.R on the raw data <<<
# I'm gonna overwrite those files to save on storage.
# The code in this script comes from code/data_exploration/explore_prices.R
  # See that script for more comments and printing intermediate output to the screen.
# the files "data_tickets_clean" are constructed in fix_codigo_viagem_all.R

# Clean-up, packages, classes and functions -------------------------------

rm( list = ls() )

library( data.table )
library( tidyverse )

# Check files, define dates -------------------------------------------------------------

list.files( "clean_data/tickets", full.names = TRUE )

# Not working on Dec-2019 and Jan-2020 for now because there are problems with those files.
dates <- c( paste0( 0, 1:9, "_2019" ),
            paste0( 10:11, "_2019" )
            )

for ( ix in seq_along( dates ) ){
  
  # >>>>> Load the data <<<<<
  
  data_tickets <- fread( input = paste0( "clean_data/tickets/data_tickets_clean_", dates[[ ix ]], ".csv" ), integer64 = "character" )
  
  # >>>>> Cases when valor_total = valor_tarifa + valor_pedagio + valor_taxa_embarque <<<<<
  
  # Check how often that's indeed the case.
  cat( "Date: ", dates[[ ix ]], ". How often does valor_tarifa include the discount? ",
       data_tickets[ , sum( abs( valor_tarifa + valor_pedagio + valor_taxa_embarque - valor_total ) < 1e-02 ) ],
       " observations out of ", nrow( data_tickets ), ".\n", "Store and clean these cases; take them out of data_tickets.\n",
       sep = "" )
  
  # Store these records.
  # First create a new observation ID in data_tickets
  data_tickets[ , obs_id := seq_len( .N ) ]
  # Then save the matching observations into their own dataset.
  data_tickets_incl_discount <- data_tickets[ abs( valor_tarifa + valor_pedagio + valor_taxa_embarque - valor_total ) < 1e-02 ]
  # Then take these observations out of data_tickets
  data_tickets <- data_tickets[ !( obs_id %in% data_tickets_incl_discount$obs_id ) ]
  
  # >>>>> Clean data_tickets_incl_discount <<<<<
  data_tickets_incl_discount[ , `:=`( valor_tarifa_sem_desconto = valor_tarifa / ( 1 - valor_percentual_desconto/100 ),
                                      # Note that this will include NaN when desconto == 100. Uninformative when valor_tarifa == 0
                                      valor_tarifa_com_desconto = valor_tarifa,
                                      valor_tarifa = NULL )
                              ]
  
  # >>>>> Cases when valor_total != valor_tarifa + valor_pedagio + valor_taxa_embarque <<<<<
  
  # How many zeros?
  # cat( "Date: ", dates[[ ix ]], ". How many cases with valor_total == 0 in the remaining data? ",
  #      data_tickets[ near( valor_total, 0 ), .N ], " out of ", nrow( data_tickets ), ".\n",
  #      sep = ""
  # )
  
  # How many records s.t. valor_tarifa does not include the discount?
  cat( "Date: ", dates[[ ix ]], ". How many records in the remaining data s.t. valor_tarifa does *not* include the discount? ",
       data_tickets[ abs( valor_tarifa * ( 1 - valor_percentual_desconto / 100 ) + valor_pedagio + valor_taxa_embarque - valor_total ) < 1e-02,
                     .N ],
       " out of ", nrow( data_tickets ), ".\n", "Fix these records.\n",
       sep = ""
  )
  # Fix these records
  # Store them in their own dataset
  data_tickets_wout_discount <- data_tickets[ abs( valor_tarifa * ( 1 - valor_percentual_desconto / 100 ) + valor_pedagio + valor_taxa_embarque - valor_total ) < 1e-02 ]
  # Take them out of data_tickets
  data_tickets <- data_tickets[ !( obs_id %in% data_tickets_wout_discount$obs_id ) ]
  # Clean them.
  data_tickets_wout_discount[ , `:=`( valor_tarifa_sem_desconto = valor_tarifa,
                                      valor_tarifa_com_desconto = valor_tarifa * ( 1 - valor_percentual_desconto / 100 ),
                                      valor_tarifa = NULL )
                              ]
  #
  
  # >>>>> Cases when valor_total = 0 <<<<<
  
  cat( "Date: ", dates[[ ix ]], ". How many records in the remaining data s.t. valor_total == 0? ",
       data_tickets[ valor_total < 1e-02, .N ], 
       " out of ", nrow( data_tickets ), ". \n",
       sep = ""
       )
  
  # Put the zeros in their own dataset
  data_tickets_zeros <- data_tickets[ valor_total < 1e-02 ]
  # Take them out of data_tickets
  data_tickets <- data_tickets[ !( obs_id %in% data_tickets_zeros$obs_id ) ]
  
  # Look at the distribution of `tipo_gratuidade`
  #cat( "Date: ", dates[[ ix ]], ". Cases with valor_total == 0: what's the distribution of `tipo_gratuidade` like? \n" )
  #print( data_tickets_zeros[ , summary( as.factor( tipo_gratitude ) ) ] )
  # Many of these really should be zero: Passe Livre, Idoso 100%.
  # Potentially problematic cases: Tarifa Normal and Tarifa Promocional.
  #cat( "Date: ", dates[[ ix ]], ". Deal with 'Tarifa Normal' cases and take them out of 'data_tickets_zeros'." )
  data_tickets_wrong_zeros <- data_tickets_zeros[ str_detect( string = tipo_gratitude, "Tarifa Normal" ) ]
  # Define new variables and fix valor_total
  data_tickets_wrong_zeros[ ,
                            `:=`( valor_tarifa_sem_desconto = valor_tarifa / ( 1 - valor_percentual_desconto/100 ),
                                  valor_tarifa_com_desconto = valor_tarifa,
                                  valor_total = valor_tarifa + valor_pedagio + valor_taxa_embarque,
                                  valor_tarifa = NULL
                            )
                            ]
  
  # Take these observations out of data_tickets_zeros
  data_tickets_zeros <- data_tickets_zeros[ !( obs_id %in% data_tickets_wrong_zeros$obs_id ) ]
  
  # Look into "Tarifa Promocional"
  #data_tickets_zeros[ str_detect( string = tipo_gratitude, "Tarifa Promocional" ) ]
  # These seem to be correct: explicit 100% discounts. Check the distribution.
  # data_tickets_zeros[ str_detect( string = tipo_gratitude, "Tarifa Promocional" ),
  #                     quantile( x = valor_percentual_desconto, probs = seq( 0, 1, 0.05 ) )
  #                     ]
  # A couple of zeros.
  # In any case, all remaining observations in `data_tickets_zeros` seem to be actual zeros.
  # So I'll just fix the discounts for all of them so everything matches.
  # Just check that there are no zero valor_tarifa [so all can be interpretd as the price without discount]
  cat( "Date: ", dates[[ ix ]], ". Check remaining zeros: how often is valor_tarifa == zero? ",
       data_tickets_zeros[ valor_tarifa < 1e-02, .N ],
       " out of ", nrow( data_tickets_zeros ), ". \n",
       sep = ""
  )
  
  # Fix.
  data_tickets_zeros[ , `:=`( valor_tarifa_sem_desconto = valor_tarifa,
                              valor_tarifa_com_desconto = valor_total - valor_taxa_embarque - valor_pedagio,
                              # Note valor_tarifa_com_desconto really is negative in a few cases [there are discounts of more than 100%]
                              valor_tarifa = NULL
  )
  ][ ,
     valor_percentual_desconto := ( valor_tarifa_sem_desconto - valor_tarifa_com_desconto ) / valor_tarifa_sem_desconto * 100
     ]
  
  # >>>>> Clean remaining observations <<<<<
  
  cat( "Date: ", dates[[ ix ]], ". Clean remaining ", nrow( data_tickets ), " observations. \n",
       sep = "" )
  
  # What problems remain?
  #data_tickets
  # The first 5 are incorrect discounts.
  # The bottom 5 have 2 records with incorrect discounts and 3 records with very small errors (on the order of 1e-02)
  # [where valor_tarifa is also *without* discount]
  
  # Decision: trust valor_total, pedagio, taxa de embarque and desconto. Fix the rest.
  cat( data_tickets[ valor_tarifa + valor_pedagio + valor_taxa_embarque < valor_total, .N ],
       " observations s.t. valor_tarifa + valor_pedagio + valor_taxa_embarque < valor_total. Fix them.\n",
       sep = "" )
  data_tickets[ valor_tarifa + valor_pedagio + valor_taxa_embarque < valor_total,
                `:=`( valor_tarifa_com_desconto = valor_total - valor_pedagio - valor_taxa_embarque )
                ][
                  valor_tarifa + valor_pedagio + valor_taxa_embarque < valor_total,
                  valor_tarifa_sem_desconto := valor_tarifa_com_desconto / ( 1 - valor_percentual_desconto / 100 )
                  ]
  
  cat( "Fix the remaining ", data_tickets[ valor_tarifa + valor_pedagio + valor_taxa_embarque >= valor_total, .N ], " observations. \n",
       sep = "" )
  
  # Now fix the discounts in all other cases
  data_tickets[ valor_tarifa + valor_pedagio + valor_taxa_embarque >= valor_total ]
  data_tickets[ valor_tarifa + valor_pedagio + valor_taxa_embarque >= valor_total,
                `:=`( valor_tarifa_com_desconto = valor_total - valor_taxa_embarque - valor_pedagio,
                      valor_tarifa_sem_desconto = valor_tarifa
                )
                ][
                  valor_tarifa + valor_pedagio + valor_taxa_embarque >= valor_total,
                  valor_percentual_desconto := ( valor_tarifa_sem_desconto - valor_tarifa_com_desconto ) / valor_tarifa_sem_desconto * 100
                  ]
  
  # Get rid of valor_tarifa
  data_tickets[ , valor_tarifa := NULL ]
  
  # >>>>> Put all datasets together <<<<<
  cat( "Put all intermediate datasets together. \n" )
  data_tickets <- rbindlist( list( data_tickets_incl_discount, data_tickets_wout_discount, data_tickets_wrong_zeros, data_tickets_zeros, data_tickets ),
                             use.names = TRUE )
  
  # Drop the other datasets bc they're not needed anymore
  rm( data_tickets_incl_discount, data_tickets_wout_discount, data_tickets_wrong_zeros, data_tickets_zeros )
  
  # >>>>> Look for NaN and impute values for them based on averages <<<<<
  
  cat( "Fix NaNs and infinites.\n" )
  
  # Impute valor_tarifa_sem_desconto as the average price for that linha/ponto_origem_viagem/ponto_destino_viagem/tipo_servico
  # First compute that average
  data_tickets[ ,
                `:=`( tarifa_media_sem_desconto = .SD[ !is.na( valor_tarifa_sem_desconto ) & is.finite( valor_tarifa_sem_desconto ), mean( valor_tarifa_sem_desconto ) ] ),
                by = .( nu_linha, ponto_origem_viagem, ponto_destino_viagem, tipo_servico )
                ]
  
  # There'll still be NaNs (probably groups with a single obs)
  # So define a coarser average
  data_tickets[
    ,
    tarifa_media_sem_desconto_v2 := .SD[ !is.na( valor_tarifa_sem_desconto ) & is.finite( valor_tarifa_sem_desconto ), mean( valor_tarifa_sem_desconto ) ],
    by = .( nu_linha, ponto_origem_viagem, ponto_destino_viagem )
  ]
  
  # Then impute that value for the NaN and infinite observations
  data_tickets[ is.nan( valor_tarifa_sem_desconto ) | is.infinite( valor_tarifa_sem_desconto ),
                valor_tarifa_sem_desconto := ifelse( !is.na( tarifa_media_sem_desconto ), tarifa_media_sem_desconto, tarifa_media_sem_desconto_v2 )
                ]
  # Note that some problems will remain [618 for Jan, hopefully small numbers for other months too]
  # ---> I could use data from other months to fix these observations <---
  
  # Drop tarifa_media_sem_desconto
  data_tickets[ , `:=`( tarifa_media_sem_desconto = NULL,
                        tarifa_media_sem_desconto_v2 = NULL 
                        ) ]
  
  # Save output -------------------------------------------------------------
  
  cat( "Save output. \n" )
  
  # >>>>> NOTE: overwrite data_tickets_clean_mm_yyyy.csv to save on storage <<<<<
  fwrite( x = data_tickets, paste0( "clean_data/tickets/data_tickets_clean_prices_", dates[[ ix ]], ".csv" ) )
  
  cat( " >>>>>>>>>> DONE <<<<<<<<<< \n" )
  
}

# Notes -------------------------------------------------------------------

# It would be good to record:
  # (i) Fraction of the data s.t. valor_tarifa includes the discount.
  # (ii) Fraction of the data s.t. valor_tarifa does *not* include the discount.
  # (iii) Fraction of the data being fixed under the "Tarifa Normal" cases.
