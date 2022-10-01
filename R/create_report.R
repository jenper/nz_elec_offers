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





