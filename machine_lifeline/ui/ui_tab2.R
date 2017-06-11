#...... ui for graphs tab .....

tabPanel(
  
  'Gauge',
  
  fluidRow(
    
    column( 8, strong( 'Read Me' ), 
            
            helpText( strong( 
              
    p( 'Quickly catch mismatch of R output ( what is expected ) and Gauge doc ( what is displayed in dataviewer ).
       In "Table Type" ( see Top right ), "Comparison Possible" means comparison between R output and gauge doc is possible.
       It will compare all machines for a particular date and summarize the possible Errors.
       There are two more tables called "Gauge Doc Table" and "R output Table" ( below the first table ). ' ),
    
    p( 'If you find an error in first table, you can check the corresponding values in "R Output Table"
and "Gauge Doc Table" to find out the root cause of the mismatch. Then file a github issue after checking R output and gauge doc.
       "Comparison Not Possible" means number of expected gauges and number of displayed gauges are not matching due to some reason.
       In such cases, pick up the company and machine name and search R output Table and Gauge Doc table to find out the root cause of mismatch.' ),
    
    
    style = "color:grey" ) ) ) ,

    
    
    column( 2, selectInput( 'tab2_selected_server', label = 'Server',
                            
                            choices = list( 'Production' = 'Production', 'Verification' = 'Verification' ), selected = 'Production' ) ),
    
    column( 2, selectInput( 'tab2_df_input', label = 'Table Type',
                            
                            choices = list( 'Comparison Possible' = 'Comparison Possible',
                                            
                                            'Comparison Not Possible' = 'Comparison Not Possible' ), selected = 'Comparison Possible' ) )
    
    ),
  
  br(),
  
  fluidRow(
    
    dataTableOutput( 'comparison_not_comparison_df' ),
    
    br(),
    
    textOutput( 'r_output_text' ),
    
    br(),
    
    dataTableOutput( 'combined_r_output_df' ),
    
    br(),
    
    textOutput( 'gaugedoc_output_text' ),
    
    br(),
    
    dataTableOutput( 'combined_gaugedoc_output_df' )
    
  )
  
)


