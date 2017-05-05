#....... Packages ......

library( twitteR ) ; library( RCurl ) ; library( RJSONIO ) ; library( stringr ) ; library( ggplot2 ) ; library( treemap )

library( 'ggthemes' )

#....... Loading lexicon of positive and negative words (from Neal Caren) .......

lexicon = read.csv( 'lexicon.csv' , stringsAsFactors = F )

#....... Scripts defined by us .......

source( 'Twitter_Authentication.R' )       #....... twitter authentication

source( 'logic_Sentiment_Analysis.R' )     #....... url related functions


