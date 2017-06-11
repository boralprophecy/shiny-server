#..... Server logic for Sensor Presence Information tab .....

#.......... Analysis ............

dataInput_gauge = reactive({
  
  selected_server = input$tab2_selected_server
  
  server_url = ifelse( selected_server == 'Production', 'http://54.162.246.37/log-V2/', 'http://52.90.124.93/log1/' )
  
  date_to_check = as.character( Sys.Date() )    #.... as gauge_doc is available for current time only
  
  company_machine_combined_names = Particular_Date_Company_Machine_Names( date_to_check, server_url )
  
  company_machine_last_json_url = Company_Machine_Last_Json_Url( company_machine_combined_names, date_to_check, server_url )
  
#  company_machine_combined_last_Routput_json_url = Company_Machine_Combined_Last_Routput_Json_Url( company_machine_combined_names, date_to_check, server_url )
  
  
  company_machine_Routput_gauge_doc_df_list = lapply( company_machine_last_json_url, function( current_company_machine_last_json_url ){
    
   company_machine_Routput_gauge_doc_df_list_output = lapply( current_company_machine_last_json_url, function( current_effective_json_url_0 ){
     
     current_effective_json_url = unlist( current_effective_json_url_0 )
     
     current_company_machine_last_json_Routput_url = paste0( current_effective_json_url, '.output' )
     

      json_data = Read_JSON_From_Server( current_company_machine_last_json_Routput_url )$gauge
      
      pm_function_names = names( json_data )
      
      #....... if there is any problem in r output ........
      
      if( ( sum( ( length( pm_function_names ) == 1 )&( !grepl( ':', pm_function_names ) ) ) != 0 )|( length( pm_function_names ) == 0 ) ){
        
        current_r_output_df = data.frame( "Comparison_Failure_Reason" = 'Data missing in JSON' )
        
      } else{
        
        i = 1 ; result_df = list()
        
        for( i in 1:length( json_data ) ){
          
          current_name_data = unlist( strsplit( gsub( ':  ', ':', pm_function_names[i] ), ':' ) )
          
          current_element = as.list( json_data[[i]] )
          
          pm_function = current_name_data[[2]]
          
          subassemblyinstance = current_name_data[[1]]
          
          value = as.character( ifelse( is.null( current_element$value ), 'Not Found', current_element$value ) )
          
          color = ifelse( is.null( current_element$color ), 'Not Found', current_element$color )
          
          mart = as.list( current_element$mart )
          
          trend = as.character( ifelse( is.null( mart$trend ), 'Not Found', mart$trend ) )
          
          yellow_threshold = as.character( ifelse( is.null( mart$yellow_threshold ), 'Not Found', mart$yellow_threshold ) )
          
          red_threshold = as.character( ifelse( is.null( mart$red_threshold ), 'Not Found', mart$red_threshold ) )
          
          trend_Y_max = as.character( ifelse( is.null( mart$trendYMax ), 'Not Found', mart$trendYMax ) )
          
          trend_Y_min = as.character( ifelse( is.null( mart$trendYMin ), 'Not Found', mart$trendYMin ) )
          
          unit = as.character( ifelse( is.null( current_element$unit ), 'Not Found', current_element$unit ) )
          
          success = ifelse( is.null( current_element$success ), 'Not Found', current_element$success )
          
          reliability_quotient = as.character( ifelse( is.null( current_element$reliabilityQuotient ), 'Not Found', current_element$reliabilityQuotient ) )
          
          publishing_sensortype = ifelse( is.null( current_element$publishingSensorType ), 'Not Found', current_element$publishingSensorType )
          
          reason = ifelse( is.null( current_element$reason), 'Not Found', current_element$reason )
          
          reason = ifelse( reason == ' ', 'Not Found', reason )
          
          hide_from_group = ifelse( is.null( current_element$hideFromGroup ) , 'Not Found', as.character( current_element$hideFromGroup ) )
          
          result_df[[i]] = data.frame( pm_function = pm_function, subassemblyinstance = subassemblyinstance,
                                       
                                       value = value, color = color, trend = trend, yellow_threshold = yellow_threshold,
                                       
                                       red_threshold = red_threshold, trend_Y_max = trend_Y_max, trend_Y_min = trend_Y_min,
                                       
                                       unit = unit, success = success, reliability_quotient = reliability_quotient, publishing_sensortype = publishing_sensortype,
                                       
                                       reason = reason, hide_from_group = hide_from_group )
          
          
        }
        
        current_r_output_df = bind_rows( result_df )
        
        current_r_output_df = mutate( current_r_output_df, subassemblyInstance_pm_function = paste0( subassemblyinstance, ':', pm_function ) )
        
      }
      
      
      #........ Getting df for gauge_doc ............
      
  #    current_effective_json_url = gsub( '.output', '', current_company_machine_last_json_Routput_url )
      
      current_effective_json = suppressWarnings( fromJSON( getURL( current_effective_json_url ) ) )     #............ JSON input of data
      
      current_machine_id = current_effective_json$header$machineId
      
      current_gauge_doc_output_df = Gauge_Doc_df( current_machine_id, tolower( selected_server ) )
      
      server_side_gauges_indices = which( current_gauge_doc_output_df$pm_function %in% server_side_gauges_names )
      
      if( length( server_side_gauges_indices ) == 0 ){
        
        current_gauge_doc_output_df_r_gauges = current_gauge_doc_output_df
        
      } else{
        
        current_gauge_doc_output_df_r_gauges = current_gauge_doc_output_df[ -server_side_gauges_indices, ]
        
      }
      
      current_gauge_doc_output_df_r_gauges = mutate( current_gauge_doc_output_df_r_gauges, subassemblyInstance_pm_function = paste0( subassemblyinstance, ':', pm_function ) )
      
      r_output_gauge_doc_comparison_df = R_Output_Gauge_Doc_Comparison_df( current_r_output_df, current_gauge_doc_output_df_r_gauges )
      
      return( list( 'r_output_df' = current_r_output_df, 'gauge_doc_output_df' = current_gauge_doc_output_df_r_gauges,
                    
                    'r_output_gauge_doc_comparison_df' = r_output_gauge_doc_comparison_df ) )
      
    })
    
    return( company_machine_Routput_gauge_doc_df_list_output )
    
  })
  
  
  Routput_Gauge_Doc_Processed_DF( company_machine_Routput_gauge_doc_df_list, key_publication_matrix )
  
})


#...... Comparison / Not comparison df ......

output$comparison_not_comparison_df = renderDataTable({
  
  selected_df_type = input$tab2_df_input
  
  if( selected_df_type == 'Comparison Possible' ){
    
    return( dataInput_gauge()$combined_comparison_df )
    
  } else{
    
    return( dataInput_gauge()$combined_not_comparison_df )
    
  }
  
})


#...... Combined R output df ......

output$combined_r_output_df = renderDataTable({
  
  dataInput_gauge()$combined_routput_df
  
})


#...... Combined Gauge Doc df ......

output$combined_gaugedoc_output_df = renderDataTable({
  
  dataInput_gauge()$combined_gaugedoc_df
  
})


#...... Text output ......

output$r_output_text = renderText({ '......................................................Gauge Doc Table..............................................' })

output$gaugedoc_output_text = renderText({ '............................................R Output Table..............................................' })




