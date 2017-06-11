#..... Server logic for Sensor Presence Information tab .....

#.......... Analysis ............
  
dataInput_sensor_presence_info = reactive({
  
  selected_server = input$sensor_presence_information_selected_server
  
  server_url = ifelse( selected_server == 'Production', 'http://54.162.246.37/log-V2/', 'http://52.90.124.93/log1/' )
  
  date_to_check = input$sensor_presence_information_date_input
  
  company_machine_combined_names = Particular_Date_Company_Machine_Names( date_to_check, server_url )
  
  company_machine_last_json_url = Company_Machine_Last_Json_Url( company_machine_combined_names, date_to_check, server_url )
  
  Sensor_Presence_DF( company_machine_last_json_url )
  
})

#...... Output filtered by comment type ......

output$sensor_presence_data_available_time_dataframe = renderDataTable({
  
  selected_comment_type = input$sensor_presence_information_comment_type
  
  before_filter_output_df = dataInput_sensor_presence_info()
  
  switch( selected_comment_type,
          
          'All' = before_filter_output_df,
          
          'Error' = filter( as.data.frame( before_filter_output_df ), Comment == 'Error' ),
          
          'Warning' = filter( as.data.frame( before_filter_output_df ), Comment == 'Warning' )
          
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

