#' Visualization of correlation matrix
#' 
#' Plots a correlation matrix, part numeric, part shape represented. Non-numeric columns are auto included by converting to factors. 
#' 
#' @param data a list of data.frames which should have at one column in common. 
#' @return a correlation plot
#' @examples
#' correlation(data = c(offers,demand))

correlation <- function(data = list(offers,demand)){
  
  if (!requireNamespace("corrplot", quietly = TRUE)) {
    stop("Package corrplot must be installed to use this function.",call. = FALSE)
  }
  
  #join data 
  if (class(data) == 'list'){
    tryCatch({
      if (exists('df', where = environment(), inherits=FALSE)){
        remove(df)
      }
      for (i in 1:length(data)){
        if (!exists('df', where = environment(), inherits=FALSE)){
          df = data[i]
        } else {
          df = merge(df, data[i])
        }
      }
      data = as.data.frame(df)
      assign("data", data, env=globalenv())
      remove(df)
    }, error = function(e){
      message('Failed to merge datasets, check datasets has columns in common.')
    }
    )
  }
  
  #remove date cols 
  
  #convert non numeric columns to numeric type
  tonum = function(data){
    isnum = sapply(data, is.numeric)
    notnum = (1:ncol(data))[!isnum]
    for (i in notnum){
      if (!is.factor(data[[i]])){
        data[, i] = as.numeric(factor(data[, i])) 
      }
    }
    return(data)
  }
  
  #lapply(tonum(head(data)),class)
  
  #create correlation matrix 
  data = cor(tonum(data))
  
  #plot correlation matrix 
  corrplot::corrplot.mixed(data, order = 'alphabet', tl.cex = 0.8, tl.pos ="lt")
}

