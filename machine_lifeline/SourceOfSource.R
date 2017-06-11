
to_stop_output = if( file.exists('.Rhistory') ) file.remove('.Rhistory')  #..... removing history files

#....... Packages ......

library( shiny ) ; library( ggplot2 ) ; library( dplyr ) ; library( RCurl ) ; library( RJSONIO ) ; library( XML )

#....... Reading of csv for various information .......

subassembly_message_type = read.csv( 'subassembly_message_type.csv', stringsAsFactors = F )

key_publication_matrix = read.csv( 'routput_key_publication_matrix.csv', stringsAsFactors = F )

#....... Scripts defined by us .......

source( 'Global_Functions.R' )    #..... Global functions

 source( 'tab1_Logic.R' )     #....... Tab 1 Logic

 source( 'tab2_Logic.R' )     #....... Tab 2 Logic


