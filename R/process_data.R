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
  date <- readLines(val)[3]
  # extract the date
  date <- gsub(".*,","", (date))
  date <- gsub("\"", "", date)
  
  raw_data <- read_csv(val, skip = 10, trim_ws = TRUE, col_types = "cccndnnncccc", na = c("-"))
  processed <- head(raw_data, -1)
  processed$Date <- dmy(date)
  result <- rbind(result, processed)
  }
  
  result <- result[order(result$Name),]
  result <- fill(result, Sector, .direction = c("up"))
    
  file_in_dir <- paste(category, ".RData", sep = "")
    
    if (category == "minvol"){
      minvol <- result
      name_list <- make.names(colnames(minvol), unique = TRUE)
      names(minvol) <- name_list
      save(minvol, file = paste("data", file_in_dir, sep = "/"))
    } else if (category == "usa"){
      usa <- result
      name_list <- make.names(colnames(usa), unique = TRUE)
      names(usa) <- name_list
      usa <- mutate(usa, Market.Value = Market.Value * 1000)
      save(usa, file = paste("data", file_in_dir, sep = "/"))
    } else {
      warning("The argument requires either \"minvol\" or \"usa\"")
    }
}


