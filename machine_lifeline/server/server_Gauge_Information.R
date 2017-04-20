#..... Server logic for Sensor Presence Information tab .....

#.......... Analysis ............

dataInput = reactive({
  
  date_to_check = input$gauge_information_date_input
  
  company_machine_combined_names = Particular_Date_Company_Machine_Names( date_to_check )
  
  company_machine_last_json_url = Company_Machine_Last_Json_Url( company_machine_combined_names )
  
  Gauge_Information_DF( company_machine_last_json_url )
  
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