#...... ui for graphs tab .....

tabPanel(
  
  'Gauge',
  
  fluidRow(
    
    column( 8, strong( 'Read Me' ), helpText( strong( div( 'Comparison of R output and Gauge Doc', style = "color:grey" ) ) ) ),
    
    column( 2, dateInput( 'tab2_date_input', label = 'Date', value = Sys.Date() - 1 ) )
    
    ),
  
  br(),
  
  fluidRow(
    
    dataTableOutput( 'sensor_presence_data_available_time_dataframe' ),
    
    br(),
    
    dataTableOutput( 'sensor_presence_data_available_time_dataframe' ),
    
    br(),
    
    dataTableOutput( 'sensor_presence_data_available_time_dataframe' )
    
  )
  
)


