#............ Global Functions and information .............

server_side_gauges_names = c( 'Harmonic Distortion Current', 'Harmonic Distortion Voltage', 'VRMS', 'IRMS', 'Active Power', 'Power Factor', 'Grounding Health' )

#......... To get particular date company machine names ........

Particular_Date_Company_Machine_Names = function( date_to_check ){
  
  server_url = 'http://54.162.246.37/log-V2/'
  
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


#......... To get company machine combined last Routput json url ..............

Company_Machine_Combined_Last_Routput_Json_Url = function( company_machine_combined_names ){
  
  sapply( company_machine_combined_names, function( company_machine_names ){
    
    company_machine_combinded_last_Routput_json_url_output = sapply( company_machine_names, function( current_company_machine_name ){
      
      company_machine_info = unlist( strsplit( current_company_machine_name, ';' ) )
      
      current_company = company_machine_info[1]  ;  current_machine_name = company_machine_info[2]
      
      server_url = 'http://54.162.246.37/log-V2/'
      
      url_creation = paste0( server_url, date_to_check, '/', current_company, '/', current_machine_name, '/' )   #...... url creation
      
      to_get_json_list = getHTMLLinks( url_creation )
      
      json_file_indexes = grep( '.json', to_get_json_list ) ; r_output_json_file_indexes = grep( '_routput', to_get_json_list )
      
      r_output_json_file_names = to_get_json_list[ intersect( json_file_indexes, r_output_json_file_indexes ) ]
      
      last_Routput_json_url = paste0( url_creation, r_output_json_file_names[ length( r_output_json_file_names ) ] )
      
      return( last_Routput_json_url )
      
    })
    
    return( company_machine_combinded_last_Routput_json_url_output )
    
  })
  
}


#......... Reading JSON file from server ...........

Read_JSON_From_Server = function( current_json_url ){
  
  tryCatch( {
    
    current_json = suppressWarnings( fromJSON( getURL( current_json_url ) ) )     #............ JSON input of data
    
  }, error = function( cond ){    #.... In case of error in first time, another attempt will be made for reading the json ........
    
    tryCatch( {   #.... In case of error in second time, another attempt will be made for reading the json ........
      
      current_json = suppressWarnings( fromJSON( getURL( current_json_url ) ) )     #............ JSON input of data
      
    }, error = function( cond ){
      
      tryCatch( { 
        
        current_json = suppressWarnings( fromJSON( getURL( current_json_url ) ) )     #............ JSON input of data
        
      }, error = function( cond ){  #.... In case of error in third time, another attempt will be made for reading the json ........
        
        tryCatch( {
          
          current_json = suppressWarnings( fromJSON( getURL( current_json_url ) ) )     #............ JSON input of data
          
        }, error = function( cond ){   #.... In case of error in fourth time, another attempt will be made for reading the json ........
          
          current_json = suppressWarnings( fromJSON( getURL( current_json_url ) ) )     #............ JSON input of data
          
        } )
        
      } )
      
    } )
    
  } )
  
}






