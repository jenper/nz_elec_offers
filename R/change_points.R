#' Detect change points in time series 
#' 
#' Uses the changepoint.np package which implements the multiple changepoint algorithm PELT with a nonparametric cost function 
#' based on the empirical distribution of the data. This finds points delimiting segments where the mean and variance of a 
#' series has changed. 
#' 
#' @param data data.frame that should contain columns that can form a time series. defaults to inbuilt offers data set  
#' @param x numeric vector of response data to find outlying points from. defaults to 'DollarsPerMegawattHour' column in offers data set
#' @param group optional vector to group data by
#' @return offers_changepoint class object with attribute 'changepoint' containing the indices of identified change points  
#' @examples
#' change_points(data=offers,x='Megawatts', group='TradingDate')
#' plot(change_points())

change_points <- function (data=offers,x='DollarsPerMegawattHour', group='TradingDate'){
  
  #run checks 
  if (!(all(c(x,group) %in% colnames(data)))){
    stop('columns not found in data. check column names')
  }
  
  if ((length(x)>1) | (length(group)>1)){
    stop('Check additional arguemnts are single values')
  }
  
  if (!is.numeric(data[[x]])){
    stop('Check x is numeric type')
  }  
  
  #data = data[!is.na(data[c(x, group)]),]
  
  x = dplyr::sym(x)
  if (!is.na(group)){
      group = dplyr::sym(group)
      data = data %>% dplyr::group_by(!!group) %>% dplyr::summarise(x = sum(!!x))
      #aggregate(data$x, by=list(data$TradingDate), FUN=sum)
  }
  
  #constructor
  new_offers_changepoint <- function(x, ...) {
    stopifnot(is.numeric(x))
    
    changepoints = changepoint.np::cpt.np(x, class=FALSE, nquantiles=length(data))
    #changepoints = changepoints[-length(changepoints)]
    attr(x, "changepoint") <- changepoints
    
    attr(x, "algorithm") <- 'PELT'
    
    class(x) <- "offers_changepoint"
   
    #structure(x, class = "offers_changepoint", changepoint = changepoints, algorithm = 'PELT')
    return(x)
  }
  
  #validation
  validate_offers_changepoint <- function(x) {
    x = unclass(x)
    if(any(is.na(x))) {
      stop("missing x values")
    }
    return(x)
  }
  
  #offers_changepoint <- function(x = numeric()) {
  #  x <- as.numeric(x)
  #  attr(x, "changepoint") <- changepoint.np::cpt.np(x, class=FALSE, nquantiles=length(data))
  #  offers_changepoint(x)
  #}
  
  #create class methods
  print.offers_changepoint <- function(x) {
    return(paste('Change points occurred at values', x[x$changepoint,], ', indexed at points', x$changepoint)) 
  }
  
  plot.offers_changepoint <- function(x) {
    #plot with proper date axis 
    p = plot(changepoint.np::cpt.np(x, nquantiles=length(data))) #temp placeholder
    return(p)
  }
  
  changes = data$x[-length(data$x)]
  
  attr(changes, "changepoint") <- changepoint.np::cpt.np(changes, class=FALSE, nquantiles=length(data))
  
  class(changes) <- "offers_changepoint"
  
  #segment = cpt.meanvar(data$x)
  #plot(segment)
  
  return(changes)
}
