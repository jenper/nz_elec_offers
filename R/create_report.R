#' Generates html report 
#' 
#' Generates an html file using the other functions in the package that should give the user analysis on the inputted 
#' datasets with summaries, plots and highlighting any anomalies in the data. 
#' 
#' @param data data.frame that should contain columns that can form a time series. defaults to inbuilt offers data set  
#' @param response numeric vector of the variable that should be the focus of the reports analysis, y argument in many functions. defaults to 'DollarsPerMegawattHour' from offers
#' @param report_title string of title of the report. defaults to 'Report'
#' @param output_dir get directory function or string of local path where report should be saved. defaults to current working directory
#' @param output_file string of html file name. defaults to 'report.html'
#' @return does not return an object but generates an html file.
#' @examples
#' create_report(data = offers, response = 'Megawatts')
#' create_report(data = offers, response = 'DollarsPerMegawattHour', report_title = "Report", output_dir = getwd(), output_file = "report.html")

create_report <- function(data = offers, response = 'DollarsPerMegawattHour', report_title = "Report", output_dir = getwd(), output_file = "report.html") {

  if (!is.data.frame(data)){
    tryCatch({
      data = data.frame(data)
    }, error = function(e) {
      message('Check input is a data.frame object')
      return(NULL)
    })
  }

  if (!is.null(response)) {
    if (!(response %in% colnames(data))) stop("`", response, "` not found in data")
  }
  
  ##ADJUST TO PASS CONFIG ARGUEMENTS TO PLOTS? 
  
  #make html report
  rmarkdown::render(
    input = system.file("template.Rmd", package = "offers"),
    output_format = rmarkdown::html_document(toc = TRUE, toc_depth = 6, theme = "flatly"),
    output_file = output_file,
    output_dir = output_dir,
    intermediates_dir = output_dir,
    params = list(data = data, response = response, set_title = report_title)
  )

  report_path <- path.expand(file.path(output_dir, output_file, fsep="/"))
  browseURL(report_path)
}





