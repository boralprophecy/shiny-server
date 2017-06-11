#............ Comparison of r_output and gauge_doc .............

R_Output_Gauge_Doc_Comparison_df = function( r_output_df, gauge_doc_df ){
  
  #...... Check : Same gauges are published in r_output and gauge_doc_output ........
  
  length_r_output_gauges = nrow( r_output_df ) ; length_gauge_doc_gauges = nrow( gauge_doc_df )
  
  r_output_df_empty_indicator = ifelse( length_r_output_gauges == 0, 1, 0 )      #...... 1 means empty df
  
  gauge_doc_df_empty_indicator = ifelse( length_gauge_doc_gauges == 0, 1, 0 )      #...... 1 means empty df
  
  
  
  r_output_failure_reason = ifelse( r_output_df_empty_indicator == 1, 'R output is empty.', '' )
  
  gauge_doc_failure_reason = ifelse( gauge_doc_df_empty_indicator == 1, 'Gauge Doc is empty', '' )
  
  
  if( ( r_output_df_empty_indicator == 1 )|( gauge_doc_df_empty_indicator == 1 ) ){     #....... if r_output or gauge_doc is empty
    
    comparison_df_output = data.frame( 'Comparison_Failure_Reason' = paste( r_output_failure_reason, gauge_doc_failure_reason ) )
    
  } else{    #...... if both r_output and gauge_doc are not empty
  
  
    same_gauges_published_check = ifelse( ( ( length_r_output_gauges == length_gauge_doc_gauges )&
                                            
                                            ( sum( r_output_df$subassemblyInstance_pm_function %in% gauge_doc_df$subassemblyInstance_pm_function ) == length_r_output_gauges ) ), 'No Error', 'Error' )
  
    if( same_gauges_published_check == 'No Error' ){   #...... if same number of gauges are there in gauge_doc and r_output after removing server side gauges
    
      #...... Other checks .........
    
      i = 1 ; value_match_check = NULL ; color_match_check = NULL ; trend_match_check = NULL ; yellow_threshold_match_check = NULL
    
      red_threshold_match_check = NULL ; trend_Y_max_match_check = NULL ; trend_Y_min_match_check = NULL ; unit_check = NULL ; success_check = NULL
    
      reliability_quotient_check = NULL ; publishing_sensortype_check = NULL ; reason_check = NULL ; current_subassemblyInstance_pm_function = NULL
    
      comparison_df = list() ; hide_from_group = NULL
    
      for( i in 1:length_r_output_gauges ){
      
        current_pm_gauge = r_output_df$subassemblyInstance_pm_function[i]
      
        gauge_doc_index = which( current_pm_gauge == gauge_doc_df$subassemblyInstance_pm_function )
      
        value_match_check[i] = ifelse( r_output_df$value[i] == gauge_doc_df$value[ gauge_doc_index ], ' ', 'Error' )
      
        color_match_check[i] = ifelse( r_output_df$color[i] == gauge_doc_df$color[ gauge_doc_index ], ' ', 'Error' )
      
        trend_match_check[i] = ifelse( r_output_df$trend[i] == gauge_doc_df$trend[ gauge_doc_index ], ' ', 'Error' )
      
        yellow_threshold_match_check[i] = ifelse( r_output_df$yellow_threshold[i] == gauge_doc_df$yellow_threshold[ gauge_doc_index ], ' ', 'Error' )
      
        red_threshold_match_check[i] = ifelse( r_output_df$red_threshold[i] == gauge_doc_df$red_threshold[ gauge_doc_index ], ' ', 'Error' )
      
        trend_Y_max_match_check[i] = ifelse( r_output_df$trend_Y_max[i] == gauge_doc_df$trend_Y_max[ gauge_doc_index ], ' ', 'Error' )
      
        trend_Y_min_match_check[i] = ifelse( r_output_df$trend_Y_min[i] == gauge_doc_df$trend_Y_min[ gauge_doc_index ], ' ', 'Error' )
      
        unit_check[i] = ifelse( r_output_df$unit[i] == gauge_doc_df$unit[ gauge_doc_index ], ' ', 'Error' )
      
        success_check[i] = ifelse( r_output_df$success[i] == gauge_doc_df$success[ gauge_doc_index ], ' ', 'Error' )
      
        reliability_quotient_check[i] = ifelse( r_output_df$reliability_quotient[i] == gauge_doc_df$reliability_quotient[ gauge_doc_index ], ' ', 'Error' )
      
        publishing_sensortype_check[i] = ifelse( r_output_df$publishing_sensortype[i] == gauge_doc_df$publishing_sensortype[ gauge_doc_index ], ' ', 'Error' )
      
        reason_check[i] = ifelse( r_output_df$reason[i] == gauge_doc_df$reason[ gauge_doc_index ], ' ', 'Error' )
      
        current_subassemblyInstance_pm_function[i] = r_output_df$subassemblyInstance_pm_function[i]
        
        hide_from_group[i] = ifelse( r_output_df$hide_from_group[i] == gauge_doc_df$hide_from_group[ gauge_doc_index ] , ' ', 'Error'  )
        
        comparison_df[[i]] = data.frame( 'SubassemblyInstance_with_PM_Function' = current_subassemblyInstance_pm_function[i], 'Value_Match' = value_match_check[i],
                                       
                                       'Color_Match' = color_match_check[i], 'Trend_Match' = trend_match_check[i], 'Yellow_Threshold_Match' = yellow_threshold_match_check[i],
                                       
                                       'Red_Threshold_Match' = red_threshold_match_check[i], 'Trend_Y_Max_Match' = trend_Y_max_match_check[i],
                                       
                                       'Trend_Y_Min_Match' = trend_Y_min_match_check[i], 'Unit_Match' = unit_check[i], 'Success_Match' = success_check[i],
                                       
                                       'Reliability_Quotient_Match' = reliability_quotient_check[i], 'Publishing_Sensor_Match' = publishing_sensortype_check[i], 
                                       
                                       'Reason_Match' = reason_check[i], 'Hide_From_Group_Match' = hide_from_group[i]
                                       
        )
      
      }
    
      comparison_df_output = bind_rows( comparison_df )
    
  } else{   #...... if same number of gauges are NOT there in gauge_doc and r_output after removing server side gauges
    
      comparison_df_output = data.frame( 'Comparison_Failure_Reason' = 'Gauge Doc and R Output gauges NOT matching' )
    
    }
  
  
  }
  
  return( comparison_df_output )
  
}


#.......... Gauge_doc output ..........

Gauge_Doc_df = function( machine_id, selected_server ){
  
  first_part_link = paste0( 'https://appv2.', selected_server, '.prophecysensorlytics.com:5043/api/gauge?filter={%22where%22:{%22machineId%22:%22' )
  
  last_part_link = '%22},%22include%22:[{%22relation%22:%22pmFunction%22,%22scope%22:{%22include%22:[{%22relation%22:%22groups%22},{%22relation%22:%22subAssembly%22},{%22relation%22:%22gaugeRules%22,%22scope%22:{%22include%22:[%22highlights%22,%22xUNIT%22,%22yUNIT%22,%22userRequiredInputs%22]}}]}},{%22relation%22:%22subAssemblyInstance%22,%22scope%22:{%22include%22:%22subAssembly%22}}]}'
  
  gauge_doc_link = paste0( first_part_link, machine_id, last_part_link )
  
  json0 = suppressWarnings( fromJSON( getURL( gauge_doc_link ) ) )
  
  i = 1 ; gauge_doc_df = list()
  
  for( i in 1:length( json0 ) ){
    
    pm_function = ifelse( is.null(json0[[i]]$pmFunction$name ), 'Not Found', json0[[i]]$pmFunction$name )
    
    subassemblyinstance = ifelse( is.null( json0[[i]]$subAssemblyInstance$name ), 'Not Found', json0[[i]]$subAssemblyInstance$name )
    
    value = as.character( ifelse( is.null( json0[[i]]$value ), 'Not Found', json0[[i]]$value ) )
    
    color = ifelse( is.null( json0[[i]]$color ), 'Not Found', json0[[i]]$color )
    
    mart = as.list( json0[[i]]$mart )
    
    trend = as.character( ifelse( is.null( mart$trend ), 'Not Found', mart$trend ) )
    
    yellow_threshold = as.character( ifelse( is.null( mart$yellow_threshold ), 'Not Found', mart$yellow_threshold ) )
    
    red_threshold = as.character( ifelse( is.null( mart$red_threshold ), 'Not Found', mart$red_threshold ) )
    
    trend_Y_max = as.character( ifelse( is.null( mart$trendYMax ), 'Not Found', mart$trendYMax ) )
    
    trend_Y_min = as.character( ifelse( is.null( mart$trendYMin ), 'Not Found', mart$trendYMin ) )
    
    unit = as.character( ifelse( is.null( json0[[i]]$unit ), 'Not Found', json0[[i]]$unit ) )
    
    success = ifelse( is.null( json0[[i]]$success ), 'Not Found', json0[[i]]$success )
    
    reliability_quotient = as.character( ifelse( is.null(json0[[i]]$reliabilityQuotient), 'Not Found', json0[[i]]$reliabilityQuotient ) )
    
    publishing_sensortype = ifelse( is.null(json0[[i]]$publishingSensorType), 'Not Found', json0[[i]]$publishingSensorType)
    
    reason = ifelse( is.null(json0[[i]]$reason), 'Not Found', json0[[i]]$reason)
    
    reason = ifelse( reason == ' ', 'Not Found', reason )
    
    hide_from_group = ifelse( is.null( json0[[i]]$hideFromGroup ), 'Not Found', as.character( json0[[i]]$hideFromGroup ) )
    
    gauge_doc_df[[i]] = data.frame( pm_function = pm_function, subassemblyinstance = subassemblyinstance,
                                    
                                    value = value, color = color, trend = trend, yellow_threshold = yellow_threshold,
                                    
                                    red_threshold = red_threshold, trend_Y_max = trend_Y_max, trend_Y_min = trend_Y_min,
                                    
                                    unit = unit, success = success, reliability_quotient = reliability_quotient, publishing_sensortype = publishing_sensortype,
                                    
                                    reason = reason, hide_from_group = hide_from_group )
    
    
  }
  
  gauge_doc_df_combined = bind_rows( gauge_doc_df )
  
  server_side_gauges_row_indices = which( server_side_gauges_names %in% gauge_doc_df_combined$pm_function )
  
  if( length( server_side_gauges_row_indices ) != 0 ){    #..... if there are some server side gauges
    
    gauge_doc_df_output = gauge_doc_df_combined[ -server_side_gauges_row_indices, ]
    
  } else{ gauge_doc_df_output = gauge_doc_df_combined }    #..... if there are no server side gauges
  
  
  #....... Pm function and publishing sensortype combining for selecting utilization gauges ........
  
  pm_function_publishing_sensor = paste0( gauge_doc_df_output$pm_function, ':', gauge_doc_df_output$publishing_sensortype )
  
  #......... if machine utilization pf or usage pf gauge is present delete 24 hour pump utilization and machine utilization ac gauge ...........
  
  if( ( "Machine Utilization:PF" %in% pm_function_publishing_sensor ) | ( "Usage:PF" %in% pm_function_publishing_sensor ) ){
    
    if( "Machine Utilization:AC" %in% pm_function_publishing_sensor ){    #....... if machine utilization ac gauge is present deleting it
      
      gauge_doc_df_output = gauge_doc_df_output[ -which( pm_function_publishing_sensor == "Machine Utilization:AC" ), ]
      
    }
    
    if( "24 Hour Pump Utilization:VAC" %in% pm_function_publishing_sensor ){    #....... if 24 Hour Pump Utilization vac gauge is present deleting it
      
      gauge_doc_df_output = gauge_doc_df_output[ -which( pm_function_publishing_sensor == "24 Hour Pump Utilization:VAC" ), ]
      
    }
    
  } else{
    
    if( "24 Hour Pump Utilization:VAC" %in% pm_function_publishing_sensor ){    #....... if 24 hour pump utilization vac is present delete machine utilization ac gauge
      
      if( "Machine Utilization:AC" %in% pm_function_publishing_sensor ){    #....... if machine utilization ac gauge is present deleting it
        
        gauge_doc_df_output = gauge_doc_df_output[ -which( pm_function_publishing_sensor == "Machine Utilization:AC" ), ]
        
      }
      
    }
    
  }
  
  
  return( gauge_doc_df_output )
  
}


#.......... Processed R output and gauge doc dfs .......

Routput_Gauge_Doc_Processed_DF = function( company_machine_Routput_gauge_doc_df_list, key_publication_matrix ){
  
  #........ Company level calculations .........
  
  current_company_index = 1 ; current_company_routput_df = list() ; current_company_gauge_doc_df = list()
  
  current_company_comparison_df = list() ; current_company_not_comparison_df = list()
  
  for( current_company_index in 1:length( company_machine_Routput_gauge_doc_df_list ) ){
    
    current_company_data = company_machine_Routput_gauge_doc_df_list[[ current_company_index ]]
    
    
    #........ Machine level calculations .........
    
    current_company_machine_index = 1 ; current_machine_routput_df = list() ; current_machine_gauge_doc_df = list()
    
    current_machine_comparison_df = list() ; current_machine_not_comparison_df = list()
    
    
    for( current_company_machine_index in 1:length( current_company_data ) ){
      
      current_company_machine_info = unlist( strsplit( names( current_company_data )[ current_company_machine_index ], ';' ) )
      
      current_company_name = current_company_machine_info[1]  ;  current_machine_name = current_company_machine_info[2]
      
      current_machine_company_data = current_company_data[[ current_company_machine_index ]]
      
      
      #....... R output df processing ......
      
      current_machine_routput_df_0 = mutate( current_machine_company_data$r_output_df, 'Machine' = current_machine_name, 
                                                 
                                                'Company' = current_company_name )
      
      current_machine_routput_df_names = names( current_machine_routput_df_0 )
      
      to_be_names_routput_df = c( rev( current_machine_routput_df_names )[1:2],
                                                     
         current_machine_routput_df_names[ -c( ( length( current_machine_routput_df_names ) - 1 ):length( current_machine_routput_df_names ) ) ] )
      
      current_machine_routput_df[[ current_company_machine_index ]] = current_machine_routput_df_0[ to_be_names_routput_df ]
      
      
      #....... Gauge Doc df processing ......
      
      current_machine_gaugedoc_df_0 = mutate( current_machine_company_data$gauge_doc_output_df, 'Machine' = current_machine_name, 
                                             
                                             'Company' = current_company_name )
      
      current_machine_gaugedoc_df_names = names( current_machine_gaugedoc_df_0 )
      
      to_be_names_gaugedoc_df = c( rev( current_machine_gaugedoc_df_names )[1:2],
                       
                       current_machine_gaugedoc_df_names[ -c( ( length( current_machine_gaugedoc_df_names ) - 1 ):length( current_machine_gaugedoc_df_names ) ) ] )
      
      current_machine_gauge_doc_df[[ current_company_machine_index ]] = current_machine_gaugedoc_df_0[ to_be_names_gaugedoc_df ]
      
      
      #....... R output and gauge doc Comparison df processing ......
      
      current_comparison_df_0 = mutate( current_machine_company_data$r_output_gauge_doc_comparison_df, 'Machine' = current_machine_name, 
                                              
                                              'Company' = current_company_name )
      
      current_comparison_df_names = names( current_comparison_df_0 )
      
      to_be_names_comparison_df = c( rev( current_comparison_df_names )[1:2],
                                   
                                   current_comparison_df_names[ -c( ( length( current_comparison_df_names ) - 1 ):length( current_comparison_df_names ) ) ] )
      
      current_machine_comparison_df[[ current_company_machine_index ]] = current_comparison_df_0[ to_be_names_comparison_df ]
      
      
      if( 'Comparison_Failure_Reason' %in% names( current_machine_comparison_df[[ current_company_machine_index ]] ) ){
        
        current_machine_not_comparison_df[[ current_company_machine_index ]] = current_comparison_df_0[ to_be_names_comparison_df ]
        
        current_machine_comparison_df[[ current_company_machine_index ]] = NULL
        
      } else{
        
        current_machine_not_comparison_df[[ current_company_machine_index ]] = NULL
        
      }
      
    }
    
    current_company_routput_df[[ current_company_index ]] = bind_rows( current_machine_routput_df )
    
    current_company_gauge_doc_df[[ current_company_index ]] = bind_rows( current_machine_gauge_doc_df )
    
    
    current_company_not_comparison_df[[ current_company_index ]] = bind_rows( current_machine_not_comparison_df )
      
    current_company_comparison_df[[ current_company_index ]] = bind_rows( current_machine_comparison_df )
    
    
  }
  
  
  #........ Combining the output for all companies and machines ........
  
  combined_routput_df = bind_rows( current_company_routput_df )
  
  combined_gaugedoc_df = bind_rows( current_company_gauge_doc_df )
  
  combined_comparison_df = bind_rows( current_company_comparison_df )
  
  combined_not_comparison_df = bind_rows( current_company_not_comparison_df )
  
  
  #............ Removing unnecessary 'Error' .............
  
  effective_pm_functions = sapply( combined_comparison_df$SubassemblyInstance_with_PM_Function, function( x ){
    
    return( unlist( strsplit( x, ':' ) )[2] )
    
  })
  
  names( effective_pm_functions ) = NULL
  
  columns_to_suppress_unnecessary_errors = names( key_publication_matrix )[-1]
  
  combined_comparison_df_value_part = combined_comparison_df[ , columns_to_suppress_unnecessary_errors ]
  
  #....... Muting the errors and warnings for false positive cases ..........
  
  j = 1
  
  for( j in 1:length( effective_pm_functions ) ){
    
    current_pm_function = effective_pm_functions[j]
    
    csv_row_index = which( current_pm_function == key_publication_matrix$gauge_name )
    
    key_publication_indices = unlist( key_publication_matrix[ csv_row_index, -1] )
    
    combined_comparison_df_value_part[ j, ][ key_publication_indices == 0 ] = ' '
    
  }
  
  combined_comparison_df_final = bind_cols( combined_comparison_df[ , 1:3 ], combined_comparison_df_value_part )
  
  combined_comparison_df_error_only = String_Specific_Row_selection( combined_comparison_df_final, 'Error' )
  
  return( list( 'combined_routput_df' = combined_routput_df, 'combined_gaugedoc_df' = combined_gaugedoc_df,
                
                'combined_comparison_df' = combined_comparison_df_error_only, 'combined_not_comparison_df' = combined_not_comparison_df ) )
  
}

