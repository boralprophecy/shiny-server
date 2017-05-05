#...... ui for graphs tab .....

tabPanel(
  
  'Gauge',
  
  fluidRow(
    
    column( 8, strong( 'Read Me' ), helpText( strong( div( 'Gauges related information', style = "color:grey" ) ) ) ),
    
    column( 2, dateInput( 'gauge_information_date_input', label = 'Date', value = Sys.Date() - 1 ) ),
    
    column( 2, selectInput( 'gauge_information_comment_type', label = 'Comment Type',
                            
                            choices = list( 'Error' = 'Error', 'Warning' = 'Warning', 'All' = 'All' ), selected = 'Error' ) )
    
    ),
  
  br(),
  
  fluidRow(
    
    dataTableOutput( 'sensor_presence_data_available_time_dataframe' )
    
  )
  
    )


