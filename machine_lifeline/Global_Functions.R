#............ Global Functions and information .............

server_side_gauges_names = c( 'Harmonic Distortion Current', 'Harmonic Distortion Voltage', 'VRMS', 'IRMS',
                              
                              'Active Power', 'Power Factor', 'Grounding Health', 'Altitude' )

#......... To get particular date company machine names ........

Particular_Date_Company_Machine_Names = function( date_to_check, server_url ){
  
  url_creation = paste0( server_url, date_to_check, '/' )   #...... url creation
  
  to_get_company_names = getHTMLLinks( url_creation )
  
  pre_company_names = to_get_company_names[ grep( '/', to_get_company_names ) ]
  
  company_names = pre_company_names[ - grep( '/log', pre_company_names ) ]
  
  company_names = gsub( '/', '', company_names )
  
  company_name_machine_name_combined = sapply( company_names, function( current_company_name ){
    
    url_creation_machine_name_retrieval = paste0( server_url, date_to_check, '/', current_company_name, '/' )   #...... url creation
    
    to_get_machine_names = getHTMLLinks( url_creation_machine_name_retrieval )
    
    pre_machine_names = to_get_machine_names[ grep( '/', to_get_machine_names ) ]
    
    machine_names = pre_machine_names[ - grep( '/log',pre_machine_names ) ]
    
    machine_names = gsub( '/', '', machine_names )
    
    company_name_machine_name_combined_output = machine_names
    
    company_name_machine_name_combined_output_with_company = paste0( current_company_name, ';', machine_names )
    
    return( company_name_machine_name_combined_output_with_company )
    
  })
  
  return( company_name_machine_name_combined )
  
}


#......... To get last json url for each machine .........

Company_Machine_Last_Json_Url = function( company_machine_combined_names, date_to_check, server_url ){
  
  company_machine_last_json_url_output = sapply( company_machine_combined_names, function( company_machine_names ){
    
    company_machine_last_json_url_output = sapply( company_machine_names, function( current_company_machine_name ){
      
      company_machine_info = unlist( strsplit( current_company_machine_name, ';' ) )
      
      current_company = company_machine_info[1]  ;  current_machine_name = company_machine_info[2]
      
      url_creation = paste0( server_url, date_to_check, '/', current_company, '/', current_machine_name, '/' )   #...... url creation
      
      to_get_json_list = getHTMLLinks( url_creation )
      
      json_file_indexes = grep( '.json', to_get_json_list ) ; r_output_json_file_indexes = grep( '.output', to_get_json_list )
      
      r_output_json_file_names = to_get_json_list[ intersect( json_file_indexes, r_output_json_file_indexes ) ]
      
      json_file_names = to_get_json_list[ setdiff( json_file_indexes, r_output_json_file_indexes ) ]
      
      last_json_url = paste0( url_creation, json_file_names[ length( json_file_names ) ] )
      
      return( last_json_url )
      
    })
    
    return( company_machine_last_json_url_output )
    
  })
  
  return( company_machine_last_json_url_output )
  
}



#......... To get company machine combined last Routput json url ..............

Company_Machine_Combined_Last_Routput_Json_Url = function( company_machine_combined_names, date_to_check, server_url ){
  
  sapply( company_machine_combined_names, function( company_machine_names ){
    
    company_machine_combinded_last_Routput_json_url_output = sapply( company_machine_names, function( current_company_machine_name ){
      
      company_machine_info = unlist( strsplit( current_company_machine_name, ';' ) )
      
      current_company = company_machine_info[1]  ;  current_machine_name = company_machine_info[2]
      
      url_creation = paste0( server_url, date_to_check, '/', current_company, '/', current_machine_name, '/' )   #...... url creation
      
      to_get_json_list = getHTMLLinks( url_creation )
      
      json_file_indexes = grep( '.json', to_get_json_list ) ; r_output_json_file_indexes = grep( '.output', to_get_json_list )
      
      r_output_json_file_names = to_get_json_list[ intersect( json_file_indexes, r_output_json_file_indexes ) ]
      
      if( length( r_output_json_file_names ) == 0 ){
        
        last_Routput_json_url = NULL
        
      } else{
        
        last_Routput_json_url = paste0( url_creation, r_output_json_file_names[ length( r_output_json_file_names ) ] )
        
      }
      
      return( last_Routput_json_url )
      
    })
    
    return( company_machine_combinded_last_Routput_json_url_output )
    
  })
  
}


#......... Reading JSON file from server ...........

Read_JSON_From_Server = function( current_json_url ){
  
  if( !is.null( current_json_url ) ){
    
    i = 1 ; dummy_var = 0
    
    for( i in 1:10 ){     #..... try 10 times
      
      out = tryCatch( {
        
        current_json = suppressWarnings( fromJSON( getURL( current_json_url ) ) )     #............ JSON input of data
        
        dummy_var = 1
        
      }, error = function( cond ){})
      
      if( dummy_var == 1 ){ break }
      
    }
    
    if( dummy_var == 0 ){ current_json = NULL }    #..... if fetching fails even after 10 tries
    
  } else{ current_json = NULL }
  
  return( current_json )
  
}


#......... Reading JSON file from server ...........

Read_JSON_From_Server_Unlimited_Try = function( current_json_url ){
  
  if( !is.null( current_json_url ) ){
    
    dummy_var = 0
    
    repeat({
      
      try( {
        
        current_json = suppressWarnings( fromJSON( getURL( current_json_url ) ) )     #............ JSON input of data
        
        dummy_var = 1
        
      })
      
      if( dummy_var == 1 ){ break }
      
    })
    
  } else{ current_json = NULL }
  
  return( current_json )
  
}


#......... Selecting rows in dataframe which has at least one predefined string in any cell of the row ...........

String_Specific_Row_selection = function( dataframe, search_string ){
  
  raw_row_index = apply( dataframe, 2, function( x ){
    
    return( row_index = which( x == search_string ) )
    
  })
  
  row_indices = unique( unlist( raw_row_index ) )
  
  return( dataframe[ row_indices, ] )
  
}




