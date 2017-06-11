#...... ui for Sensor Presence Information tab .....

tabPanel(
  
  'Sensor',
  
  fluidRow(
    
    column( 6, strong( 'Read Me' ), helpText( strong( div( 'Displays sensor information for latest batch of selected date.
                                    "Error" is shown when mismatch between
                                    "Sensors Found" and "Possible Sensors" is fatal. "Warning" is shown when this mismatch is not fatal but
                                    should get noted. You can select "Comment Type" to find a particular type of comment quickly.', style = "color:grey" ) ) ) ),
    
    column( 2, selectInput( 'sensor_presence_information_selected_server', label = 'Server',
                            
                            choices = list( 'Production' = 'Production', 'Verification' = 'Verification' ), selected = 'Production' ) ),
    
    column( 2, dateInput( 'sensor_presence_information_date_input', label = 'Date', value = Sys.Date() - 1 ) ),
    
    column( 2, selectInput( 'sensor_presence_information_comment_type', label = 'Comment Type',
                            
                            choices = list( 'Error' = 'Error', 'Warning' = 'Warning', 'All' = 'All' ), selected = 'Error' ) )
    
  ),
  
  br(),
  
  fluidRow(
   
    dataTableOutput( 'sensor_presence_data_available_time_dataframe' )
    
  )
  
)


