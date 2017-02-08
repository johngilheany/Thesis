rel_weight <- function(category){
  
  summary <- data.frame();
  
  if (category == "minvol"){
    data(minvol)
    dates <- unique(minvol$Date)
    temp <- minvol
  } else 
    if (category == "usa"){
    data(usa)
    dates <- unique(usa$Date)
    temp <- usa
  }
  
  for (date in dates){
    # Extract entries with the matched date
    msci_by_date <- minvol[which(temp$Date == date),]
    
    # Calculate the relative weight of each sector for the given date
    sector.freq <- table(msci_by_date$Sector)
    sector.relfreq <- sector.freq/nrow(msci_by_date)
    sector.weight <- sector.relfreq[which(sector.relfreq > 0)]
    
    local_summary<- merge(date, sector.weight)
    summary <- rbind(summary, local_summary)
  }
  colnames(summary) <- c("Date", "Sector", "Weight" )
  summary$Date <- as.Date(summary$Date, origin = "1970-01-01")
  summary$Sector <- as.character(summary$Sector)
  summary <- mutate(summary, Sector = ifelse(Sector == "Telecommunications", "Telecommunication Services", Sector))
  
  if (category == "minvol"){
    minvol_weight <- summary
    save(minvol_weight, file = "C:/Users/John Gilheany/Documents/MSCI-master/msci/data/minvol_weight.RData")
  } else 
    if (category == "usa") {
    usa_weight <- summary
    save(usa_weight, file = "C:/Users/John Gilheany/Documents/MSCI-master/msci/data/usa_weight.RData")
  }
}   