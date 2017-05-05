#..... Server logic for Sensor Presence Information tab .....

#.......... Analysis ............

date_to_check = Sys.Date()

dataInput = reactive({
  
  company_machine_combined_names = Particular_Date_Company_Machine_Names( date_to_check )
  
  company_machine_combined_last_Routput_json_url = Company_Machine_Combined_Last_Routput_Json_Url( company_machine_combined_names )
  
  company_machine_Routput_gauge_doc_df_list = lapply( company_machine_combined_last_Routput_json_url, function( company_last_json_Routput_url ){
    
    company_machine_Routput_gauge_doc_df_list_output = lapply( company_last_json_Routput_url, function( current_company_machine_last_json_Routput_url ){
      
      json_data = Read_JSON_From_Server( current_company_machine_last_json_Routput_url )
      
      pm_function_names = names( json_data )
      
      i = 1; result_df = list()
      
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
        
        result_df[[i]] = data.frame( pm_function = pm_function, subassemblyinstance = subassemblyinstance,
                                     
                                     value = value, color = color, trend = trend, yellow_threshold = yellow_threshold,
                                     
                                     red_threshold = red_threshold, trend_Y_max = trend_Y_max, trend_Y_min = trend_Y_min,
                                     
                                     unit = unit, success = success, reliability_quotient = reliability_quotient, publishing_sensortype = publishing_sensortype,
                                     
                                     reason = reason )
        
        
      }
      
      current_r_output_df = bind_rows( result_df )
      
      
      #........ Getting df for gauge_doc ............
      
      current_effective_json_url = gsub( '_routput', '', current_company_machine_last_json_Routput_url )
      
      current_effective_json = suppressWarnings( fromJSON( getURL( current_effective_json_url ) ) )     #............ JSON input of data
      
      current_machine_id = current_effective_json$header$machineId
      
      current_gauge_doc_output_df = Gauge_Doc_df( current_machine_id )
      
      server_side_gauges_indices = which( current_gauge_doc_output_df$pm_function %in% server_side_gauges_names )
      
      current_gauge_doc_output_df_r_gauges = current_gauge_doc_output_df[ -server_side_gauges_indices, ]
      
      current_r_output_df = mutate( current_r_output_df, subassemblyInstance_pm_function = paste0( subassemblyinstance, ':', pm_function ) )
      
      current_gauge_doc_output_df_r_gauges = mutate( current_gauge_doc_output_df_r_gauges, subassemblyInstance_pm_function = paste0( subassemblyinstance, ':', pm_function ) )
      
      r_output_gauge_doc_comparison_df = R_Output_Gauge_Doc_Comparison_df( current_r_output_df, current_gauge_doc_output_df_r_gauges )
      
      return( list( 'r_output_df' = current_r_output_df, 'gauge_doc_output_df' = current_gauge_doc_output_df_r_gauges,
                    
                    'r_output_gauge_doc_comparison_df' = r_output_gauge_doc_comparison_df ) )
      
    })
    
    return( company_machine_Routput_gauge_doc_df_list_output )
    
  })
  
})

#...... Output filtered by comment type ......

output$sensor_presence_data_available_time_dataframe = renderDataTable({
  
  selected_comment_type = input$sensor_presence_information_comment_type
  
  before_filter_output_df = dataInput()
  
  switch( selected_comment_type,
          
          'All' = before_filter_output_df,
          
          'Error' = filter( before_filter_output_df, Comment == 'Error' ),
          
          'Warning' = filter( before_filter_output_df, Comment == 'Warning' )
          
  )
  
})



if( F ){
  
  # Anything that calls autoInvalidate will automatically invalidate
  # every 2 seconds.
  autoInvalidate <- reactiveTimer(60000)
  
  observe({
    # Invalidate and re-execute this reactive expression every time the
    # timer fires.
    autoInvalidate()
    
    
    sensor_presence_data_available_time_dataframe = renderUI({ 
      
      company_machine_last_json_url =  sapply( Particular_Date_Company_Machine_Names(), function( company_machine_names ){
        
        company_machine_last_json_url_output = sapply( company_machine_names, function( current_company_machine_name ){
          
          company_machine_info = unlist( strsplit( current_company_machine_name, ';' ) )
          
          current_company = company_machine_info[1]  ;  current_machine_name = company_machine_info[2]
          
          server_url = 'http://54.162.246.37/log-V2/'
          
          date_to_check = Sys.Date() - 1
          
          url_creation = paste0( server_url, date_to_check, '/', current_company, '/', current_machine_name, '/' )   #...... url creation
          
          to_get_json_list = getHTMLLinks( url_creation )
          
          json_file_indexes = grep( '.json', to_get_json_list ) ; r_output_json_file_indexes = grep( '_routput', to_get_json_list )
          
          r_output_json_file_names = to_get_json_list[ intersect( json_file_indexes, r_output_json_file_indexes ) ]
          
          json_file_names = to_get_json_list[ setdiff( json_file_indexes, r_output_json_file_indexes ) ]
          
          last_json_url = paste0( url_creation, json_file_names[ length( json_file_names ) ] )
          
          return( last_json_url )
          
        })
        
        return( company_machine_last_json_url_output )
        
      })
      
      ss = Sensor_Presence_Data_Available_Time_Dataframe( company_machine_last_json_url )
      
    })
    
    # Do something each time this is invalidated.
    # The isolate() makes this observer _not_ get invalidated and re-executed
    # when input$n changes.
    print(paste("The value of input$n is", isolate(input$n)))
  })
  
}

