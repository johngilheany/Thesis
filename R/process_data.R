#' @export
download_data <- function(){
  dates <- as.character(read.table("C:/Users/Nam Nguyen/Documents/MSCI-master/msci/inst/extdata/release_date.txt")$V1)
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
  file_name <- paste(as.Date(date), category, ".csv", sep = "")
  dest <- paste("C:/Users/Nam Nguyen/Documents/MSCI-master/msci/inst/extdata", category, file_name, sep = "/")
  download.file(source, dest)
}

#' @export
process_data <- function(category) {
  min_vol_paths <- dir(link <- paste("inst/extdata", category, sep = "/"), pattern = "\\.csv$", full.names = TRUE)
  result <- data.frame()
  
  for (val in min_vol_paths){
    raw_data <- readLines(val)
    
    # extract the date
    date <- raw_data[3]
    date <- gsub(".*,","", (date))
    date <- gsub("\"", "", date)
    
    # extract the data
    trimmed <- raw_data[-c(1:10)]
    trimmed2 <- head(trimmed, -1)
    processed <- read.csv(textConnection(trimmed2), header = TRUE, stringsAsFactors = FALSE)
    processed$Date <- date
    result <- rbind(result, processed)
  }
    result <- result[order(result$Ticker),]
    file_in_dir = paste(category, ".csv", sep = "")
    write.csv(result, paste("inst/extdata/processed_data", category, file_in_dir, sep = "/"), row.names = FALSE)
}


