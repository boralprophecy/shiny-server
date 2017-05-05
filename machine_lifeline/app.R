library( shiny )

source( 'SourceOfSource.R' )

ui = navbarPage(
  
  theme = 'bootstrap_slate.css',
  
  title = 'Watchdog',
  
  #..... Include ui for each tab .....
  
  source( file.path( "ui", "ui_tab1.R" ),  local = T )$value,    #..... ui for sensor presence information tab

  
  source( file.path( "ui", "ui_tab2.R" ),  local = T )$value    #..... ui for gauge information tab
  
)

server = function( input, output, session ){
  
  #..... Include server logic for each tab .....
  
  source( file.path( 'server', 'server_tab1.R' ), local = T )$value     #...... server logic for sensor presence information tab
  
  source( file.path( 'server', 'server_tab2.R' ), local = T )$value           #...... server logic for gauge information tab
  
}

shinyApp( ui = ui, server = server )

