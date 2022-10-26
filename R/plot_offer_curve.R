#' Plot offer curve
#' 
#' Plots offer curve with accumulated offers amounts, default megawatts, in the x axis and the values the offers, 
#' default dollars per megawatt hours, were made at in the y axis. Plots with the group option plots the offer curve 
#' for each group. 
#' 
#' @param data data.frame that should contain at least two numeric columns. defaults to inbuilt offers data set  
#' @param x a numeric vector that should be able to be cumulatively summed by param y. defaults to 'Megawatts' from offers 
#' @param y a numeric vector with matching param x values. defaults to 'DollarsPerMegawattHour' from offers 
#' @param group a vector which the data can be grouped by. defaults to NULL
#' @return a ggplot object plotting an offer curve
#' @examples
#' plot_offer_curve(data = offers, x='Megawatts', y='DollarsPerMegawattHour', group='ParticipantCode')
#' plot_offer_curve(data = offers, x='DollarsPerMegawattHour', y='Megawatts', group='ParticipantCode')

plot_offer_curve <- function(data = offers, x='Megawatts', y='DollarsPerMegawattHour', group=NULL) {
  #check data
  if (!is.data.frame(data)){
    tryCatch({
      data = as.data.frame(data)
    }, error = function(e) {
      stop('Check input is a data.frame object')
    })
  }

  #Include error checking for unquoted arguments 
  
  if (!(all(c(x,y) %in% colnames(data)))){
    stop('columns not found in data. check column names')
  }

  if (!is.numeric(data[[x]])){
    stop('argument x needs to be of numeric type')
  }

  #Error check for group consisting of only one observation?
  
  #plot
  #implement scale_fill_discrete for small numer of unique values 
  x = dplyr::sym(x)
  y = dplyr::sym(y)
  if (is.null(group)){
    data = data %>% dplyr::group_by(!!y) %>% dplyr::summarise(X = sum(!!x), .groups = 'drop') %>% dplyr::mutate(X = cumsum(X))
    p = ggplot2::ggplot(data=data, ggplot2::aes(x = X, y = !!y)) +
      ggplot2::geom_line(size = 0.8) +
       ggplot2::xlab(x) +
        ggplot2::theme_minimal()
  } else {
    if (!(group %in% colnames(data))){
      stop(paste(group, "not found in data. check column names"))
    }
    group = dplyr::sym(group)
    data = data %>% dplyr::group_by(!!y, !!group) %>% dplyr::summarise(X = sum(!!x), .groups = 'drop')
    data = data %>% dplyr::group_by(!!group) %>% dplyr::mutate(X = cumsum(X))
    p = ggplot2::ggplot(data=data, ggplot2::aes(x = X, y = !!y, color=!!group)) +
      ggplot2::geom_line(size = 0.8) +
       ggplot2::xlab(x) +
        ggplot2::theme_minimal()
  }
  return(p)
}

