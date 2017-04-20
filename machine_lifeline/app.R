library( shiny )

source( 'SourceOfSource.R' )

ui = navbarPage(
  
  theme = 'bootstrap_slate.css',
  
  title = 'Watchdog - Lets watch',
  
  #..... Include ui for each tab .....
  
  source( file.path( "ui", "ui_Sensor_Presence_Information.R" ),  local = T )$value,    #..... ui for sensor presence information tab

  source( file.path( "ui", "ui_Gauge_Information.R" ),  local = T )$value    #..... ui for gauge information tab
  
)

server = function( input, output, session ){
  
  #..... Include server logic for each tab .....
  
  source( file.path( 'server', 'server_Sensor_Presence_Information.R' ), local = T )$value     #...... server logic for sensor presence information tab
  
  source( file.path( 'server', 'server_Gauge_Information.R' ), local = T )$value           #...... server logic for gauge information tab
  
}

shinyApp( ui = ui, server = server )

