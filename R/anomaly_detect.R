anomaly_detect <- function(df = offers, generator=NULL, tradingperiod=NULL){
  cols = c('TradingDate','TradingPeriod','ParticipantCode','PointOfConnection', 'Unit', 'Tranche','Megawatts', 'DollarsPerMegawattHour', 'Datetime')
  if (!identical(colnames(df),cols)){
    error('Data.frame contents does not match required format. Double check data.frame has correct columns from documentation.')
  }

  #more group options here
  #by generator
  #by trading period

  #group all
  df.grp = df %>% dplyr::group_by(TradingDate) %>% dplyr::summarise(totalOffers = sum(DollarsPerMegawattHour)/1000, .groups = 'drop')

  df.grp = df.grp %>% tibble::as.tibble() %>% dplyr::mutate(TradingDate = as.Date(TradingDate))
  df.grp %>% anomalize::time_decompose(totalOffers, method = "stl", frequency = "auto", trend = "auto") %>%
    anomalize::anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.5) %>%
    anomalize::plot_anomaly_decomposition()

  recomposed = df.grp %>% anomalize::time_decompose(totalOffers) %>%
    anomalize::anomalize(remainder) %>%
    anomalize::time_recompose()

  recomposed %>% anomalize::plot_anomalies(time_recomposed = TRUE, ncol = 4, alpha_dots = 0.5)

  anomalies = recomposed %>% dplyr::filter(anomaly == 'Yes')

  return(c(anomalies$TradingDate))
}



