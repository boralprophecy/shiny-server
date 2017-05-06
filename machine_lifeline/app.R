library( shiny )

source( 'SourceOfSource.R' )

ui = navbarPage(
  
  theme = 'bootstrap_slate.css',
  
  title = 'Machine Watchdog',
  
  #..... Include ui for each tab .....
  
  source( file.path( "ui", "ui_Sensor_Presence_Information.R" ),  local = T )$value,    #..... ui for graphs tab

  source( file.path( "ui", "tab2.R" ),  local = T )$value    #..... ui for tab2
  
)

server = function( input, output, session ){
  
  #..... Include server logic for each tab .....
  
  source( file.path( 'server', 'server_Sensor_Presence_Information.R' ), local = T )$value     #...... server logic for graphs tab
  
  source( file.path( 'server', 'tab2.R' ), local = T )$value           #...... server logic for tab2
  
}

shinyApp( ui = ui, server = server )

