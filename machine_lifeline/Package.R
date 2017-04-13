rm( list = ls() )

options( warn = -1 )    #..... To suppress all warning messages globally

required_packages = list( 'RJSONIO', 'dplyr', 'XML', 'ggplot2', 'shiny' )

dummy = lapply( required_packages, function( x ){
  
                       if( !( x %in% installed.packages() ) ) { install.packages( x, repos = "http://cloud.r-project.org/", dependencies = T ) }
  
                } )
