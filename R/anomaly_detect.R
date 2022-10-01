
anomaly_detect <- function(data = offers, x = Datetime, y = VWAP, gsplit=FALSE){
  #check data
  if (!is.data.frame(data)){
    tryCatch({
      data = data.frame(data)
    }, error = function(e) {
      message('Check input is a data.frame object')
      return(NULL)
    }, warning = function(e) {
      message('Check input is a data.frame object')
      return(NULL)
    })
  }
  
  x = dplyr::enquo(x)
  y = dplyr::enquo(y)
  
  #set up for offer data, otherwise leave generic
  cols = c('TradingDate', 'TradingPeriod','ParticipantCode','Tranche','Megawatts', 'DollarsPerMegawattHour')
  if ((all(cols %in% colnames(data))) & (deparse(substitute(x))=='Datetime') & (deparse(substitute(y))=='VWAP')){
    data = tidyr::fill(data, .direction = 'updown')
    data[data['DollarsPerMegawattHour'] == 0, 'DollarsPerMegawattHour'] = 0.01
    #data = data %>% dplyr::group_by(Datetime, ParticipantCode) %>% dplyr::mutate(VWAP = (cumsum(Megawatts*DollarsPerMegawattHour))/(cumsum(Megawatts)), .keep='used') 
    if (!gsplit){
      data.grp = data %>% dplyr::group_by(Datetime) %>% dplyr::summarise(VWAP = sum(Megawatts*DollarsPerMegawattHour)/sum(Megawatts)) 
    } else {
      data.grp = data %>% dplyr::group_by(Datetime, ParticipantCode) %>% dplyr::summarise(VWAP = sum(Megawatts*DollarsPerMegawattHour)/sum(Megawatts))
    }
  #CODE TO HANDLE GROUP WITH gsplit=TRUE  
  #CODE TO HANDLE DIFFERENT ARGUEMENT X,Y WITH data=offers
  } else {
    tryCatch({
      data.grp = data %>% dplyr::group_by(!!x) %>% dplyr::summarise(y = sum(!!y))
    }, error = function(e) {   
      stop('Invalid x and y columns. Check x and y are in data')
    })
    data.grp = data %>% dplyr::group_by(!!x) %>% dplyr::summarise(y = sum(!!y))
  }

  tryCatch({
    data.grp = data.grp %>% tibble::as_tibble() %>% 
    dplyr::mutate(x = lubridate::ymd_hms(!!x)) 
  }, error = function(e) {
    message('Check x values are a date and y values are numeric format')
    return(NULL)
  })
  
  data.grp = data.grp %>% tibble::as_tibble() %>% 
    dplyr::mutate(x = lubridate::ymd_hms(!!x)) %>% 
    dplyr::arrange(x) %>%
    tibbletime::as_tbl_time(index = x)
  
  decomposed = data.grp %>% anomalize::time_decompose(!!y, method = "stl", frequency = "auto", trend = "auto") %>%
    anomalize::anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.1) 
    
  decomposed_plot = decomposed %>% anomalize::plot_anomaly_decomposition()
  
  recomposed = data.grp %>% anomalize::time_decompose(!!y) %>%
    anomalize::anomalize(remainder) %>%
    anomalize::time_recompose()

  recomposed_plot = recomposed %>% anomalize::plot_anomalies(time_recomposed = TRUE, ncol = 4, alpha_dots = 0.5)

  anomalies = recomposed %>% dplyr::filter(anomaly == 'Yes')
  
  attr(anomalies, "class") <- "anomaly"
  anomalies$decompose = decomposed_plot
  anomalies$recompose = recomposed_plot
  
  return(anomalies)
}


# print.anomaly <- function(obj){
#   data.frame(obj$x, obj$observed)
# }
# 
# plot.anomaly <- function(obj, decom=TRUE) {
#   if (decom){
#     return(obj$decompose)
#   } else {
#     return(obj$recompose)
#   }
# }