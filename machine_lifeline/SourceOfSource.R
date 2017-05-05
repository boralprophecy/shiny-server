#....... Packages ......

library( shiny ) ; library( ggplot2 ) ; library( XML ) ; library( RJSONIO ) ; library( dplyr ) ; library( RCurl )

#....... Reading of csv for various information .......

subassembly_message_type = read.csv( 'subassembly_message_type.csv', stringsAsFactors = F )

#....... Scripts defined by us .......

source( 'Global_Functions.R' )    #..... Global functions

source( 'tab1_Logic.R' )     #....... Tab 1 Logic

source( 'tab2_Logic.R' )     #....... Tab 2 Logic

source( 'MachineLifelineMain.R' )


