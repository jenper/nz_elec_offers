#' Detect anomalies in time series 
#' 
#' Uses the anomalize package to run anomaly detection using the Seasonal Hybrid ESD algorithm, decomposing time 
#' series data into its components, plotting them and identifying outlying points in red. 
#' 
#' @param data data.frame that should contain columns that can form a time series. defaults to inbuilt offers data set  
#' @param x date format vector. defaults to 'Datetime' column in offers data set
#' @param y numeric vector of response data to find outlying points from. defaults to 'VWAP' column in offers data set
#' @return anomaly class object with data on outlying points and plots of recomposed and decomposed data 
#' @examples
#' anomaly_detect(data = offers, x = 'Datetime', y = 'Megawatts', gsplit=FALSE)

anomaly_detect <- function(data = offers, x = 'Datetime', y = 'VWAP'){ #, gsplit=FALSE){
  #check data
  if (!is.data.frame(data)){
    tryCatch({
      data = data.frame(data)
    }, error = function(e) {
      stop('Check input is a data.frame object')
    }, warning = function(e) {
      stop('Check input is a data.frame object')
    })
  }
  
  if (!(all(c(x, y) %in% colnames(data)))){
    if (!(x %in% colnames(data))){
      stop('Check x and y are in data')
    }
    if (y != 'VWAP'){
      stop('Check x and y are in data')
    }
  }
  
  if (y != 'VWAP'){
    if (!(is.numeric(data[[y]]))){
      tryCatch({
        data[y] = as.numeric(df[[y]])
      }, error = function(e) {
        stop('Check y values are a numeric format')
      }, warning = function(e) {
        stop('Check y values are a numeric format')
      })
    }
  }
  
  if (!(lubridate::is.Date(data[[x]]))){
    tryCatch({
      as.Date(offers[[x]])
    }, error  = function(e) {
      stop('Check x values are a date')
    })
  }
  
  x = dplyr::sym(x)
  y = dplyr::sym(y)
  
  ##modify code for try catch around whole block
  #set up for offer data, otherwise leave generic
  cols = c('TradingDate', 'TradingPeriod','ParticipantCode','Tranche','Megawatts', 'DollarsPerMegawattHour')
  if ((all(cols %in% colnames(data))) & (y == 'VWAP')){
    data = tidyr::fill(data, .direction = 'updown')
    data[data['DollarsPerMegawattHour'] == 0, 'DollarsPerMegawattHour'] = 0.01
    #data = data %>% dplyr::group_by(Datetime, ParticipantCode) %>% dplyr::mutate(VWAP = (cumsum(Megawatts*DollarsPerMegawattHour))/(cumsum(Megawatts)), .keep='used') 
    #if (!gsplit){
    data.grp = data %>% dplyr::group_by(Datetime) %>% dplyr::summarise(VWAP = sum(Megawatts*DollarsPerMegawattHour)/sum(Megawatts)) 
    #} #else {
      #data.grp = data %>% dplyr::group_by(Datetime, ParticipantCode) %>% dplyr::summarise(VWAP = sum(Megawatts*DollarsPerMegawattHour)/sum(Megawatts))
    #}
  #CODE TO HANDLE GROUP WITH gsplit=TRUE  
  } else {
    tryCatch({
      data.grp = data %>% dplyr::group_by(!!x) %>% dplyr::summarise(y = sum(!!y))
    }, error = function(e) {   
      stop('Invalid x and y columns.')
    })
    data.grp = data %>% dplyr::group_by(!!x) %>% dplyr::summarise(y = sum(!!y))
  }
  
  # tryCatch({
  #   data.grp = data.grp %>% tibble::as_tibble() %>%
  #   dplyr::mutate(x = lubridate::ymd_hms(!!x))
  # }, error = function(e) {
  #   message('Invalid x and y columns. Check x and y are in data. Check x values are a date and y values are numeric format')
  #   return(NULL)
  # })

  data.grp = data.grp %>% tibble::as_tibble() %>%
    dplyr::mutate(x = lubridate::ymd_hms(!!x)) %>% ##error check for other date formats?
    dplyr::arrange(x) %>%
    tibbletime::as_tbl_time(index = x)

  decomposed = suppressMessages(data.grp %>% anomalize::time_decompose(y, method = "stl", frequency = "auto", trend = "auto") %>%
    anomalize::anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.1))

  decomposed_plot = decomposed %>% anomalize::plot_anomaly_decomposition()

  recomposed = suppressMessages(data.grp %>% anomalize::time_decompose(y) %>%
    anomalize::anomalize(remainder) %>%
    anomalize::time_recompose())

  recomposed_plot = recomposed %>% anomalize::plot_anomalies(time_recomposed = TRUE, ncol = 4, alpha_dots = 0.5)

  anomalies = recomposed %>% dplyr::filter(anomaly == 'Yes')

  attr(anomalies, "class") <- "anomaly"
  anomalies$decompose = decomposed_plot
  anomalies$recompose = recomposed_plot

  return(anomalies)
}

