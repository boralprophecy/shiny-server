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


#......... To get sensor presence and last time data came for each company-machine combination

Sensor_Presence_DF = function( company_machine_last_json_url ){
  
  zz = sapply( company_machine_last_json_url, function( current_company_machine_last_json_url ){
    
    yy = sapply( current_company_machine_last_json_url, function( current_json_url ){
      
      current_json = Read_JSON_From_Server( current_json_url )
      
      subassembly_instance_list = names( current_json[['subassemblies']] )
      
      if( length( subassembly_instance_list ) != 0 ){        #..... If there are at least one subassembly instance
        
        i = 1 ; subassembly_instance_sensor_info_output = NULL
        
        for( i in 1:length( subassembly_instance_list ) ){
          
          subassembly_instance = subassembly_instance_list[i]
          
          collector_names = names( current_json[["subassemblies"]][[subassembly_instance]][["collectors"]] )
          
          if( length( collector_names ) == 0 ){ collector_names = 'No sensors found' }
          
          subassembly_instance_sensor_info = paste( as.character( collector_names ), collapse="," ) ; names( subassembly_instance_sensor_info ) = subassembly_instance
          
          subassembly_instance_sensor_info_output = c( subassembly_instance_sensor_info_output, subassembly_instance_sensor_info )

        }
        
      } else{ subassembly_instance_sensor_info_output = 'No Subassembly Instances found' }
      
      return( subassembly_instance_sensor_info_output )
      
    }, simplify = F )
    
    return( yy )
    
  })
  
  j = 1 ; df_output = list() ; current_company_output = list() ; rownames_final_current_company_output = NULL
  
  for( j in 1:length( zz ) ){
    
    current_company_subassembly_instance_sensors_present = zz[[j]]
    
    company_machine_names = names( current_company_subassembly_instance_sensors_present )
    
    k = 1  ; current_company_subassembly_instance_sensors_present_output = list()
    
    for( k in 1:length( current_company_subassembly_instance_sensors_present ) ){       #..... For each company
      
      current_company_machine_name = unlist( strsplit( company_machine_names[k], ';' ) )
      
      current_company_name = current_company_machine_name[1] ; current_machine_name = current_company_machine_name[2]
      
      current_machine_subassembly_instance_sensor_list = current_company_subassembly_instance_sensors_present[[k]]
      
      current_machine_subassembly_instance_names = names( current_machine_subassembly_instance_sensor_list )
      
      if( is.null( current_machine_subassembly_instance_names ) ){ current_machine_subassembly_instance_names = "No Subassembly Instances found" }
      
      l = 1 ; current_machine_subassembly_instance_sensors_present_output = list()
      
      for( l in 1:length( current_machine_subassembly_instance_sensor_list ) ){     #.... Data frame of subassembly instances for each machine. Each row is a subassembly instance
        
        current_machine_subassembly_instance_sensors_present_output[[l]] = 
          
          data.frame( 'Company' = current_company_name, 'Machine' = current_machine_name, 'Subassembly_Instance' = current_machine_subassembly_instance_names[l],

                      'Sensors_Found' = current_machine_subassembly_instance_sensor_list[l] )
        
      }
      
      current_company_subassembly_instance_sensors_present_output[[k]] = bind_rows( current_machine_subassembly_instance_sensors_present_output )
        
    }
    
    current_company_output[[j]] = as.data.frame( bind_rows( current_company_subassembly_instance_sensors_present_output ) )
    
  }
  
  final_current_company_output = bind_rows( current_company_output )
  
  #....... Calculation for sensor matching ........
  
  row_index = 1 ; sensor_matching_df = list()
  
  for( row_index in 1:nrow( final_current_company_output ) ){
    
    current_subassembly_instance_lowercase = tolower( final_current_company_output$Subassembly_Instance[ row_index ] )
    
    current_subassembly_lowercase = gsub( '[[:digit:]]', '', current_subassembly_instance_lowercase )   #..... removing the number to get the subassembly name in lowercase
    
    csv_matching_row_index = which( current_subassembly_lowercase == subassembly_message_type$subassembly_lowercase )
    
    current_sensors_found = final_current_company_output$Sensors_Found[ row_index ]
    
    effective_possible_sensors = subassembly_message_type$possible_sensors[ csv_matching_row_index ]
    
    effective_possible_sensors_vector = unlist( strsplit( effective_possible_sensors, ',' ) )
    
    if( ( current_sensors_found == 'No sensors found' )|( current_sensors_found == 'No Subassembly Instances found' ) ){
      
      sensor_matching_df[[ row_index ]] = data.frame( 'Possible_Sensors' = effective_possible_sensors, 'Comment' = 'Error' )
      
    } else{
      
      current_sensors_found_vector = unlist( strsplit( final_current_company_output$Sensors_Found[ row_index ], ',' ) )
      
      if( sum( effective_possible_sensors_vector %in% current_sensors_found_vector ) == length( effective_possible_sensors_vector ) ){   #.... If match is found
        
        sensor_matching_df[[ row_index ]] = data.frame( 'Possible_Sensors' = effective_possible_sensors, 'Comment' = '' )
        
      } else{   #....... if mismatch found
        
        message_to_show = subassembly_message_type$message_type[ csv_matching_row_index ]
        
        sensor_matching_df[[ row_index ]] = data.frame( 'Possible_Sensors' = effective_possible_sensors, 'Comment' = message_to_show )
        
      }
      
    }
    
  }
  
  final_current_company_output_with_sensor_matching_info = bind_rows( sensor_matching_df )
  
  final_sensor_information_output = bind_cols( final_current_company_output, final_current_company_output_with_sensor_matching_info )
  
  final_sensor_information_output$Company = gsub( '%20', ' ', final_sensor_information_output$Company )  #.... Replacing %20 with space
  
  final_sensor_information_output$Machine = gsub( '%20', ' ', final_sensor_information_output$Machine )  #.... Replacing %20 with space
  
  names( final_sensor_information_output ) = gsub( '_', ' ', names( final_sensor_information_output ) )  #.... Replacing _ with space
  
  return( final_sensor_information_output )
  
}


#......... Reading JSON file from server ...........

Read_JSON_From_Server = function( current_json_url ){
  
  tryCatch( {
    
    current_json = suppressWarnings( lapply( readLines( current_json_url, n=1L ), fromJSON ) )[[1]]     #............ JSON input of data
    
  }, error = function( cond ){    #.... In case of error in first time, another attempt will be made for reading the json ........
    
    tryCatch( {   #.... In case of error in second time, another attempt will be made for reading the json ........
      
      current_json = suppressWarnings( lapply( readLines( current_json_url, n=1L ), fromJSON ) )[[1]]     #............ JSON input of data
      
    }, error = function( cond ){
      
      tryCatch( { 
        
        current_json = suppressWarnings( lapply( readLines( current_json_url, n=1L ), fromJSON ) )[[1]]     #............ JSON input of data
        
      }, error = function( cond ){  #.... In case of error in third time, another attempt will be made for reading the json ........
        
        tryCatch( {
          
          current_json = suppressWarnings( lapply( readLines( current_json_url, n=1L ), fromJSON ) )[[1]]     #............ JSON input of data
          
        }, error = function( cond ){   #.... In case of error in fourth time, another attempt will be made for reading the json ........
          
          current_json = suppressWarnings( lapply( readLines( current_json_url, n=1L ), fromJSON ) )[[1]]     #............ JSON input of data
          
        } )
        
      } )
      
    } )
    
  } )
  
}



#......... To get company names for a particular date ........

Particular_Date_Company_Names = function(){ 
  
  server_url = 'http://54.162.246.37/log-V2/'
  
  date_to_check = Sys.Date() - 1
  
  url_creation = paste0( server_url, date_to_check, '/' )   #...... url creation
  
  to_get_company_names = getHTMLLinks( url_creation )
  
  pre_company_names = to_get_company_names[ grep( '/', to_get_company_names ) ]
  
  company_names = pre_company_names[ - grep( '/log', pre_company_names ) ]
  
  company_names = gsub( '/', '', company_names )
  
  as.vector( company_names )
  
}

#......... To get machine names for a particular company ........

Particular_Company_Machine_Names = function( company_name ){
  
  url_creation = paste0( server_url, date_to_check, '/', company_name, '/' )   #...... url creation
  
  to_get_machine_names = getHTMLLinks( url_creation )
  
  pre_machine_names = to_get_machine_names[ grep( '/', to_get_machine_names ) ]
  
  machine_names = pre_machine_names[ - grep( '/log',pre_machine_names ) ]
  
  machine_names = gsub( '/', '', machine_names )
  
  return( machine_names )
  
}

#......... To get JSON list for a particular machine .......

Particular_Machine_JSON_List = function( server_name, date_to_check, company_name, machine_name ){
  
  if( server_name == 'Verification' ){ server_url = 'http://appv2.verification.prophecysensorlytics.com/log1/' }
  
  if( server_name == 'Production' ){ server_url = 'http://appv2.production.prophecysensorlytics.com/log-V2/' }
  
  url_creation = paste0( server_url, date_to_check, '/', company_name, '/', machine_name, '/' )   #...... url creation
  
  to_get_json_list = getHTMLLinks( url_creation )
  
  json_file_names = to_get_json_list[ grep( '.json', to_get_json_list ) ]
  
  json_urls = paste0( url_creation, json_file_names )
  
  json_read = lapply( json_urls, function( current_json ){
    
                             json0 = suppressWarnings( lapply( readLines( current_json, n=1L ), fromJSON ) )     #............ JSON input of data

                             return( json0 )
  } )
  
  names( json_read ) = gsub( '.json', '', json_file_names )
  
  return( json_read )
  
}

#......... To get names of NULL JSON ......

Get_NULL_JSON_Names = function( json_list ){
  
  null_json_indicator = sapply( json_list, function( current_json ){
    
                                          return( ifelse( length( current_json ) == 0, T, F ) )
  })
  
  if( sum( null_json_indicator ) == 0 ){     #..... If there is no null json
    
    null_json_names = NULL
    
  } else{
    
    null_json_names = paste( names( json_list )[ null_json_indicator ], collapse=", " )
    
  }
  
  return( null_json_names )
  
}

#......... To get names of JSON with missing subassemblyInstances .....

Get_Subassembly_Instance_Missing_JSON_Names = function( non_null_json_list ){
  
  subassembly_instances_missing_indicator = sapply( non_null_json_list, function( current_json ){
    
                                               return( ifelse( length( current_json[['subassemblies']] ) == 0, T, F ) )
    
  })
  
  if( sum( subassembly_instances_missing_indicator ) == 0 ){     #..... If there is no json with missing subassembly instances
    
    subassembly_instances_missing_json_names = NULL
    
  } else{
    
    subassembly_instances_missing_json_names = paste( names( non_null_json_list )[ subassembly_instances_missing_indicator ], collapse=", " )
    
  }
  
  return( subassembly_instances_missing_json_names )
  
}

