correlation <- function(data = list(offers,demand)){
  
  #join data 
  if (class(data) == 'list'){
    tryCatch({
      if (exists('df', where = environment(), inherits=FALSE)){
        remove(df)
      }
      for (i in length(data)){
        if (!exists('df', where = environment(), inherits=FALSE)){
          df = data[i]
        } else {
          df = merge(df, data[i])
        }
      }
      data = df
      remove(df)
    }, error = function(e){
      message('Failed to merge datasets, check datasets has columns in common.')
    }
    )
  }
  
  #placeholder correlation chart 
  return(NULL)
}
