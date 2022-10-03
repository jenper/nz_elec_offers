#' Formats EMI files 
#' 
#' Formats files from the EA emi website, specifically the offers (https://www.emi.ea.govt.nz/Wholesale/Datasets/BidsAndOffers/Offers) 
#' and reconciliation (https://www.emi.ea.govt.nz/Wholesale/Datasets/Volumes/Reconciliation) files to create data that 
#' will work with the functions in this package. These files contain generation offers and demand data on NZ electricity. 
#' 
#' @param path string of path accepts path to file or folder of files from the Offers and Reconciliation folders on the EMI website 
#' @return a data.frame of file contents formatted for use
#' @examples
#' format_data('C:/Users/user/Downloads/ReconciledInjectionAndOfftake_202111_20220626_123724.csv.gz')
#' format_data('C:/Users/user/Downloads/Offers/20211106_Offers.csv')
#' format_data('C:/Users/user/Downloads/Offers')

format_data <- function(path){
  
  #check if folder and merge contents
  tryCatch({
    if (exists('df', where = environment(), inherits=FALSE)){
      remove(df)
    }
    if (file_test("-d", path)){
      ##convert for loop to c++
      for (file in list.files(path)){
        if (!exists('df', where = environment(), inherits=FALSE)){
          df = read.csv(paste(path, file, sep='/'))
        } else {
          df2 = read.csv(paste(path, file, sep='/'))
          df = rbind(df, df2)
        }
      }
      ##
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
  
  #remove unwanted rows  
  offercols = c('TradingDate','TradingPeriod','ParticipantCode','PointOfConnection', 'Unit', 'ProductType', 'ProductClass', 'ReserveType', 'ProductDescription'
                ,'UTCSubmissionDate', 'UTCSubmissionTime', 'SubmissionOrder', 'IsLatestYesNo', 'Tranche', 'MaximumRampUpMegawattsPerHour', 'MaximumRampDownMegawattsPerHour'
                ,'PartiallyLoadedSpinningReservePercent', 'MaximumOutputMegawatts', 'ForecastOfGenerationPotentialMegawatts', 'Megawatts', 'DollarsPerMegawattHour')
  
  demandcols = c('PointOfConnection', 'Network', 'Island', 'Participant', 'TradingDate', 'TradingPeriod', 'TradingPeriodStartTime', 'FlowDirection', 'KilowattHours')
  
  #can extend code here to include other emi files 
  if (identical(colnames(df), offercols)){
    df = df[(df['ProductClass'] == 'Injection') & (df['ProductType'] == 'Energy') & (df['IsLatestYesNo'] == 'Y'), c('TradingDate','TradingPeriod', 'ParticipantCode','PointOfConnection',
                                                                                                                    'Unit', 'Tranche', 'Megawatts', 'DollarsPerMegawattHour')]
  } else if (identical(colnames(df), demandcols)){
    df = df %>% dplyr::group_by(TradingDate, TradingPeriod) %>% dplyr::summarise(MegawattHours = sum(KilowattHours)/1000, .groups = 'drop')
  } else {
    stop('File contents does not match offers or reconciliation data format. Double check using correct files from EMI website, link in documentation.')
  }
  
  #transform trading periods
  df['time'] = rcpp_time(df$TradingPeriod)
  df['Datetime'] = paste(df$TradingDate, df$time, sep=" ")
  df['Datetime'] = sapply(df['Datetime'],function(x) strptime(x,format='%Y-%m-%d %H:%M:%S'))
  df = subset(df, select = -c(time))
  df = df[with(df, order(Datetime)),]
  row.names(df) = NULL
  return(df)
}

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