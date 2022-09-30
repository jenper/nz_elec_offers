
Rcpp::cppFunction('
std::vector<std::string> rcpp_time(std::vector<double> TP) {
    int hours, minutes;
    std::vector<std::string> time(TP.size());
    for(size_t i = 0; i != TP.size(); i++) {
        TP[i] = (TP[i]-1)*30;
        hours = std::floor(TP[i] / 60);
        minutes = fmod(TP[i],60);
        time[i] = std::to_string(hours) + ":" + std::to_string(minutes) + ":00";
    }
   return time;
}')

format_data <- function(path){
  #check if folder and merge contents
  tryCatch({
    if (exists('df', where = environment(), inherits=FALSE)){
      remove(df)
    }
    if (file_test("-d", path)){
      for (file in list.files(path)){
        if (!exists('df', where = environment(), inherits=FALSE)){
            df = read.csv(paste(path, file, sep='/'))
         } else {
            df2 = read.csv(paste(path, file, sep='/'))
            df = rbind(df, df2)
         }
      }
      if (exists('df2', where = environment(), inherits=FALSE)){
        remove(df2)
      }
    } else {
      df = read.csv(path)
    }
    #df <<- df
    }, error = function(e){
    message('File or folder path not properly provided. Refer to function documentation for appropriate input format.')
    }
  )

  cols = c('TradingDate','TradingPeriod','ParticipantCode','PointOfConnection', 'Unit', 'ProductType', 'ProductClass', 'ReserveType', 'ProductDescription'
               ,'UTCSubmissionDate', 'UTCSubmissionTime', 'SubmissionOrder', 'IsLatestYesNo', 'Tranche', 'MaximumRampUpMegawattsPerHour', 'MaximumRampDownMegawattsPerHour'
               ,'PartiallyLoadedSpinningReservePercent', 'MaximumOutputMegawatts', 'ForecastOfGenerationPotentialMegawatts', 'Megawatts', 'DollarsPerMegawattHour')
  if (!identical(colnames(df),cols)){
    error('File contents does not match offers data format. Double check using correct file.')
  }

  #remove unwanted rows
  df = df[(df['ProductClass'] == 'Injection') & (df['ProductType'] == 'Energy') & (df['IsLatestYesNo'] == 'Y'), c('TradingDate','TradingPeriod', 'ParticipantCode','PointOfConnection',
  'Unit', 'Tranche', 'Megawatts', 'DollarsPerMegawattHour')]
  #transform trading periods
  df['time'] = rcpp_time(df$TradingPeriod)
  df['Datetime'] = paste(df$TradingDate, df$time, sep=" ")
  df['Datetime'] = sapply(df['Datetime'],function(x) strptime(x,format='%Y-%m-%d %H:%M:%S'))
  df = subset(df, select = -c(time))
  df = df[with(df, order(Datetime)),]
  row.names(df) = NULL
  return(df)
}



