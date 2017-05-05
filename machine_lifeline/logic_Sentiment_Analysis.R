#...... Logic of Sentiment Analysis tab .........

#...... Function to check if there are any invalid multibyte string in a character vector ......

has.invalid.multibyte.string  <- function(x,return.elements=F)
{
  # determine if "invalid multibyte string" error will be triggered
  # if return.elements=T, then output is logical along x, otherwise single logical
  if (is.null(x))
    return(F)
  if (return.elements)
  {
    n <- length(x)
    out <- rep(F,n)
    for (i in 1:n)
      out[i] <- is.error(try(toupper(x[i]),silent = T))
  }
  else
    out <- is.error(try(toupper(x),silent = T))
  return(out)
}

is.error <- function(x)
{
  # test output of try()
  return(class(x)[1]=="try-error")
}

#........ Function to clean text .........

clean.text <- function(some_txt)
{  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)

# Remove the text which start from "@"
some_txt = gsub("@\\w+", "", some_txt)

# Remove punctuations
some_txt = gsub("[[:punct:]]", "", some_txt)

#Remove Digits
some_txt = gsub("[[:digit:]]", "", some_txt)

#Remove links
some_txt = gsub("http\\w+", "", some_txt)

# remove extra white spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)


# Remove non-english characters
some_txt = gsub("[^\x20-\x7E]", "", some_txt)

# define "tolower error handling" function
try.tolower = function(x)
{  y = NA
try_error = tryCatch(tolower(x), error=function(e) e)
if (!inherits(try_error, "error"))
  y = tolower(x)
return(y)
}

some_txt = sapply(some_txt, try.tolower)
some_txt = some_txt[some_txt != ""]
names(some_txt) = NULL
return(some_txt)}

#.......... Function for finding sentiment score ........

score.sentiment = function(sentences, pos.words, neg.words,negation.words, .progress='none')
{
  library(plyr)
  library(stringr)
  
  
  nscores = laply(sentences, function(sentence, pos.words, neg.words, negation.words) {
    
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # compare our words with list of negation words
    negation=match(words,negation.words)
    
    
    # match() returns the position of the matched term or NA
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    negation=!is.na(negation)
    
    # calculation of score
    score= sum(pos.matches)-sum(neg.matches)
    
    # calculation for negation handling
    score1=score
    if(score<0)
      nscore=score1+sum(negation)
    else 
      nscore=score1-sum(negation)
    return(nscore)
  }, pos.words, neg.words,negation.words, .progress=.progress )
  
  scores.df = data.frame(score=nscores, text=sentences)
  return(scores.df)
}


#...... Calculation .........

Sentiment_Output = function( twitter_query, number_of_tweets_to_fetch ){

  tweets_list = searchTwitter( twitter_query, n = number_of_tweets_to_fetch, lang = 'en' )

  tweets_df = twListToDF( tweets_list )


  #....... Row indexes which has invalid multibyte stings .......

  invalid_row_indexes = which( has.invalid.multibyte.string( tweets_df$text, return.elements = T ) )

  final_tweets_df = tweets_df[ -invalid_row_indexes, ]

  tweets = final_tweets_df$text

  duplicated_tweets_removed = tweets[ !duplicated( tweets ) ]

  tweets_cleaned = clean.text( duplicated_tweets_removed )

  tweets_cleaned_duplicates_removed = tweets_cleaned[ !duplicated( tweets_cleaned ) ]

  

  positive_words = lexicon$word[ lexicon$polarity == 'positive' ]

  negative_words = lexicon$word[ lexicon$polarity == 'negative' ]

  negation_words = NULL

  sentiment_score_df = score.sentiment( tweets_cleaned_duplicates_removed, positive_words, negative_words, negation_words )

  total_rows = nrow( sentiment_score_df )
  
  positive_score_percent = round( 100*length( which( sentiment_score_df$score > 0 ) )/total_rows, 2 )
  
  neutral_score_percent = round( 100*length( which( sentiment_score_df$score == 0 ) )/total_rows, 2 )
  
  negative_score_percent = round( 100*length( which( sentiment_score_df$score < 0 ) )/total_rows, 2 )
  
  sentiment_distribution = data.frame( values = c( positive_score_percent, neutral_score_percent, negative_score_percent ),
                                       
                                       sentiment = c( 'Positive', 'Neutral', 'Negative' ) )
  
  return( list( 'sentiment_score_df' = sentiment_score_df, 'sentiment_distribution' = sentiment_distribution ) )
  
}


#...... Tweets and Sentiment DF for output .........

Fetched_Tweets_Sentiment_DF = function( sentiment_score_df ){
  
  tweet_sentiment = ifelse( sentiment_score_df$score > 0, 'Positive', ifelse( sentiment_score_df$score == 0, 'Neutral', 'Negative' ) )
  
  sentiment_df = data.frame( Tweet = sentiment_score_df$text, 'Tweet Sentiment' = tweet_sentiment )
  
  return( sentiment_df )
  
}


#...... Pie chart ........

#....... Function for donut and pie together ........

#' x      numeric vector for each slice
#' group  vector identifying the group for each slice
#' labels vector of labels for individual slices
#' col    colors for each group
#' radius radius for inner and outer pie (usually in [0,1])
#' plot_title title over plot

donuts <- function(x, group = 1, labels = NA, col = NULL, radius = c( 2.5, 3 ), title = NA ) {
  group <- rep_len(group, length(x))
  ug  <- unique(group)
  tbl <- table(group)[order(ug)]
  
  col <- if (is.null(col))
    seq_along(ug) else rep_len(col, length(ug))
  col.main <- Map(rep, col[seq_along(tbl)], tbl)
  col.sub  <- lapply(col.main, function(x) {
    al <- head(seq(0, 1, length.out = length(x) + 2L)[-1L], -1L)
    Vectorize(adjustcolor)(x, alpha.f = al)
  })
  
  plot.new()
  
  par(new = TRUE)
  pie(x, border = NA, radius = radius[2L],
      col = unlist(col.sub), labels = labels, main = title )
  
  par(new = TRUE)
  pie(x, border = NA, radius = radius[1L],
      col = unlist(col.main), labels = NA)
}

Tweet_Sentiment_Pie_Chart = function( sentiment_distribution, twitter_query ){
  
  plot_title = paste0( 'Pie Chart of Sentiment Analysis for ', twitter_query )
  
  return(
    
    with( sentiment_distribution, donuts( values, group = sentiment, labels = sentiment, radius = c( 0.7, 1 ),
                                          
                                          col = c( 'green', 'orange', 'red' ), title = plot_title ) )
    
  )
  
}


#....... Bar Plot ........

Tweet_Sentiment_Bar_Plot = function( sentiment_distribution, twitter_query ){
  
  plot_title = paste0( 'Bar Diagram of Sentiment Analysis for ', twitter_query )
  
  bar_plot_output = ggplot( sentiment_distribution, aes( x = sentiment, y = values, fill = sentiment ) ) +
    
    geom_bar( stat = 'identity', width = 0.6 ) + scale_fill_manual( values = c( "firebrick1", "orange", "green" ) ) + guides( fill = F ) +
    
    xlab( 'Sentiment' ) + ylab( 'Percentage' ) + ggtitle( plot_title ) +
    
    theme( plot.title = element_text( size = 12, colour = "firebrick", face = 'bold' ), axis.text = element_text( colour = "firebrick", face = 'bold' ), 
           
           axis.title.y = element_text( size = 12, face = 'bold' ), axis.title.x = element_text(size = 12, face = 'bold') ) # +
    
  #  theme_solarized_2( light = F ) + scale_colour_solarized( "black" )
  
  return( bar_plot_output )
  
}


#....... Treemap ........

Tweet_Sentiment_Treemap = function( sentiment_distribution, twitter_query ){
  
  plot_title = paste0( 'Treemap of Sentiment Analysis for ', twitter_query )
  
  return(
    
    treemap( sentiment_distribution, index = 'sentiment', vSize = 'values', vColor = 'values',
             
             type = 'value', title = plot_title )
    
  )
  
}






