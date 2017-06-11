error_row_selection = function(dataframe, search_string ){
  
  raw_row_index = apply( dataframe,2,function(x){
    
    row_index = which( x == 'Error')
    
    return( row_index )
    
  })
  
  row_indices = unique( unlist( raw_row_index ) )
  
  selected_row_dataframe = dataframe[ row_indices, ]
  
  return( selected_row_dataframe )
  
}

s = read.csv("D:/ps_software/watchdog_scripts_csv/example.csv")

aed = error_row_selection(s, 'Error')
