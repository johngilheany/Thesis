asset.info <- function(ticker = "", date = "", category = ""){
  date <- as.Date(date)
  if (category == "usa"){
    if (ticker %in% usa$Ticker && date %in% usa$Date) return(filter(usa, Ticker == ticker, Date == date))
    else {
      message("Ticker is not found")
      return(NULL)
    }
  }
  else if (category == "minvol"){
    if (ticker %in% minvol$Ticker && date %in% minvol$Date) return(filter(minvol, Ticker == ticker))
    else {
      message("Ticker is not found")
      return(NULL)
    }
  }
  else {
    warning("category must either be \"usa\" or \"minvol\"")
  }
}

graphic <- function(ticker = "", category = "", period){
  if (!is.vector(period) || length(period) != 2) stop("period has to be a vector, format: yyyy-mm-dd, yyyy-mm-dd")
  lower <- ymd(period[1])
  upper <- ymd(period[2])
  if (lower > upper) stop("invalid time period")
  
  if (category == "usa"){
    df <- filter(usa, Ticker == ticker, Date < upper, Date > lower)
    ggplot(data = df) + geom_line(mapping = aes(x = Date, y = Market.Value))
    ggplot(data = df) + geom_line(mapping = aes(x = Date, y = Shares))
  } else if (category == "minvol"){
    df <- filter(minvol, Ticker == ticker, Date < upper, Date > lower)
    ggplot(data = df) + geom_line(mapping = aes(x = Date, y = Market.Value))
    ggplot(data = df) + geom_line(mapping = aes(x = Date, y = Shares))
  } else {
    stop("category must be either \"usa\" or \"minvol\"")
  }
}
 
                
