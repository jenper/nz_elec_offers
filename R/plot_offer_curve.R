#  plot_curve(x=Megawatts, y=DollarsPerMegawattHour, group=ParticipantCode)
#  plot_curve(x=Megawatts, y=DollarsPerMegawattHour, group=TradingPeriod)
#  plot_curve(x=Megawatts, y=DollarsPerMegawattHour, group=PointofConnection)

plot_offer_curve <- function(data = offers, x=Megawatts, y=DollarsPerMegawattHour, group=NULL) {
  #check data
  if (!is.data.frame(data)){
    tryCatch({
      data = data.frame(data)
    }, error = function(e) {
      message('Check input is a data.frame object')
      return(NA)
    })
  }

  if (!(all(c(deparse(substitute(x)),deparse(substitute(y))) %in% colnames(data)))){
    stop('columns not found in data. check column names')
  }

  if (!is.numeric(data[[deparse(substitute(x))]])){
    stop('argument x needs to be of numeric type')
  }

  #plot
  x = dplyr::enquo(x)
  y = dplyr::enquo(y)
  #x = sym(x)
  #y = sym(y)
  if (is.null(data[[deparse(substitute(group))]])){
    data = data %>% dplyr::group_by(!!y) %>% dplyr::summarise(X = sum(!!x), .groups = 'drop') %>% dplyr::mutate(X = cumsum(X))
    p = ggplot2::ggplot(data=data, ggplot2::aes(x = X, y = !!y)) +
      ggplot2::geom_line(size = 0.8) +
       ggplot2::xlab(x) +
        ggplot2::theme_minimal()
  } else {
    if (!(deparse(substitute(group)) %in% colnames(data))){
      stop(paste(deparse(substitute(group)), "not found in data. check column names"))
    }
    group = dplyr::enquo(group)
    data = data %>% dplyr::group_by(!!y, !!group) %>% dplyr::summarise(X = sum(!!x), .groups = 'drop')
    data = data %>% dplyr::group_by(!!group) %>% dplyr::mutate(X = cumsum(X))
    p = ggplot2::ggplot(data=data, ggplot2::aes(x = X, y = !!y, color=!!group)) +
      ggplot2::geom_line(size = 0.8) +
       ggplot2::xlab(x) +
        ggplot2::theme_minimal()
  }
  return(p)
}

