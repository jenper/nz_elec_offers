
report <- function(df = offers){
  print('Offer data metrics:')
  print(summary(df))

  #print('Offer stack:')
  #df.grp = as.data.frame(df %>% dplyr::group_by(Datetime) %>% dplyr::summarise(totalOffers = sum(DollarsPerMegawattHour)/1000, .groups = 'drop'))
  #plot(x = df.grp['Datetime'], y = df.grp['totalOffers'])

  print('Anomalies detected are:')
  print(offers::anomaly_detect(df))
}

