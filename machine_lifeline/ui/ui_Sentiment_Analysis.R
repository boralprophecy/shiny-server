#...... ui for Sentiment Analysis tab .....

tabPanel(
  
  'Sentiment Analysis',
  
  fluidRow(
    
    column( 3, textInput( 'tab1_query', label = 'Topic', value = 'nlp' ) ),
    
    column( 3, numericInput( 'tab1_no_of_tweets_to_fetch', label = 'Number of Tweets to Fetch', value = 100 ) )
    
  ),
  
  br(),
  
  fluidRow(
    
    column( 6, plotOutput( 'tab1_pie_chart', click = 'plot_click', hover = 'plot_hover', brush = 'plot_brush' ) ),
    
    column( 6, plotOutput( 'tab1_bar_plot', click = 'plot_click', hover = 'plot_hover', brush = 'plot_brush' ) )
    
  ),
  
  br(),
  
  fluidRow(
    
    plotOutput( 'tab1_treemap', click = 'plot_click', hover = 'plot_hover', brush = 'plot_brush' )
    
  ),
  
  br(),
  
  fluidRow(
    
    dataTableOutput( 'tab1_fetched_tweets_df' )
    
  )
  
)


