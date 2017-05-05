#....... Packages ......

library( shiny ) ; library( ggplot2 ) ; library( XML ) ; library( RJSONIO ) ; library( dplyr )

#....... Reading of csv for various information .......

subassembly_message_type = read.csv( 'subassembly_message_type.csv', stringsAsFactors = F )

#....... Scripts defined by us .......

source( 'UrlProcessing.R' )     #....... url related functions

source( 'MachineLifelineMain.R' )

source( 'Additional_Functions.R' )    #..... Additional functions for coloring cells in data frame

