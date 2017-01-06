#' @export
download_data <- function(){
  dates <- as.character(read.table(system.file("extdata", "release_date.txt", package = "msci"))$V1)
  path_min_vol <- "https://www.ishares.com/us/products/239695/ishares-msci-usa-etf/1467271812596.ajax?fileType=csv&fileName=EUSA_holdings&dataType=fund&asOfDate="
  path_usa <- "https://www.ishares.com/us/products/239693/ishares-msci-usa-etf/1467271812596.ajax?fileType=csv&fileName=EUSA_holdings&dataType=fund&asOfDate="
  for (val in dates) {
    
    url_min_vol <- paste(path_min_vol, val, sep = "")
    download_helper(url_min_vol, val, "minvol")
  }
  
  for (val in dates){
    url_usa <- paste(path_usa, val, sep = "")
    download_helper(url_usa, val, "usa")
  }
}

download_helper <- function(source, date, category){
  date <- date
  file_name <- paste(as.Date(date), category, ".csv", sep = "")
  dest <- paste(system.file("extdata", package = "msci"), file_name, sep = "/")
  download.file(source, dest)
}
