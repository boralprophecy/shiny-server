#..... Server logic for Sentiment Analysis tab .....

#.......... Analysis ............
  
dataInput = reactive({
  
  twitter_query = input$tab1_query
  
  number_of_tweets_to_fetch = input$tab1_no_of_tweets_to_fetch
  
  sentiment_output = Sentiment_Output( twitter_query, number_of_tweets_to_fetch )
  
  sentiment_info = c( sentiment_output, list( 'twitter_query' = twitter_query, 'number_of_tweets_to_fetch' = number_of_tweets_to_fetch ) )
  
  return( sentiment_info )
  
})


#...... Output filtered by comment type ......

output$tab1_fetched_tweets_df = renderDataTable({
  
  sentiment_score_df = dataInput()$sentiment_score_df
  
  Fetched_Tweets_Sentiment_DF( sentiment_score_df )
  
})


#...... Pie Chart .......

output$tab1_pie_chart = renderPlot({
  
  sentiment_distribution = dataInput()$sentiment_distribution
  
  twitter_query = dataInput()$twitter_query
  
  Tweet_Sentiment_Pie_Chart( sentiment_distribution, twitter_query )
  
})


#...... Bar Chart ......

output$tab1_bar_plot = renderPlot({
  
  sentiment_distribution = dataInput()$sentiment_distribution
  
  twitter_query = dataInput()$twitter_query
  
  Tweet_Sentiment_Bar_Plot( sentiment_distribution, twitter_query )
  
})


#...... Tree Map ......

output$tab1_treemap = renderPlot({
  
  sentiment_distribution = dataInput()$sentiment_distribution
  
  twitter_query = dataInput()$twitter_query
  
  Tweet_Sentiment_Treemap( sentiment_distribution, twitter_query )
  
})
















