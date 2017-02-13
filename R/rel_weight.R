#' This function calculates the relative weight of each sector for each day 
#' and display the result in a dataframe
#' @export
rel_weight <- function(category){

  summary <- data.frame();
  
  if (category == "minvol"){
    data(minvol)
    dates <- unique(minvol$Date)
    temp <- minvol
  } else if (category == "usa"){
    data(usa)
    dates <- unique(usa$Date)
    temp <- usa
    }

  
  #Min Vol calculations
  for (date in dates){
    # Extract entries with the matched date
    msci_minvol_by_date <- minvol[which(temp$Date == date),]
    
    # Calculate the relative weight of each sector for the given date
    sector <- table(msci_minvol_by_date$Sector)
    sector.freq <- sector[names(sector) == msci_minvol_by_date$Sector]
    sector.relfreq <- sector.freq/nrow(msci_minvol_by_date)
    sector.weight <- sector.relfreq[which(sector.relfreq > 0)]

    local_summary<- merge(date, sector.weight)
    summary_minvol <- rbind(summary, local_summary)
  }
    colnames(summary_minvol) <- c("Date", "Sector", "Weight" )
    summary_minvol$Date <- as.Date(summary_minvol$Date, origin = "1970-01-01")
    summary_minvol$Sector <- as.character(summary_minvol$Sector)
    summary_minvol <- mutate(summary_minvol, Sector = ifelse(Sector == "Telecommunications", "Telecommunication Services", Sector))
   
    #USA Calculations 
    for (date in dates){
      # Extract entries with the matched date
      msci_usa_by_date <- usa[which(temp$Date == date),]
      
      # Calculate the relative weight of each sector for the given date
      sector <- table(msci_usa_by_date$Sector)
      sector.freq <- sector[names(sector) == msci_usa_by_date$Sector]
      sector.relfreq <- sector.freq/nrow(msci_usa_by_date)
      sector.weight <- sector.relfreq[which(sector.relfreq > 0)]
      
      local_summary<- merge(date, sector.weight)
      summary_usa <- rbind(summary, local_summary)
    }
    colnames(summary_usa) <- c("Date", "Sector", "Weight" )
    summary_usa$Date <- as.Date(summary_usa$Date, origin = "1970-01-01")
    summary_usa$Sector <- as.character(summary_usa$Sector)
    summary_usa <- mutate(summary_usa, Sector = ifelse(Sector == "Telecommunications", "Telecommunication Services", Sector))
    
    if (category == "minvol"){
      minvol_weight <- summary_minvol
    } else if (category == "usa"){
        usa_weight <- summary_usa
      }
}



