#' This function calculates the relative weight of each sector for each day 
#' and display the result in a dataframe
#' @export
compute_rel_weight <- function(){

  dates <- unique(usa$Date)
  summary <- character()
  for (date in dates){
    # Extract entries with the matched date
    if (date == "31-May-2013") next
    msci_by_date <- usa[which(usa$Date == date),]
    
    # Calculate the relative weight of each sector for the given date
    sector.freq <- table(msci_by_date$Sector)
    sector.relfreq <- sector.freq/nrow(msci_by_date)
    sector.weight <- sector.relfreq[which(sector.relfreq > 0)]
    
    # Merge the result into the summary data frame
    local_summary<- merge(date, sector.weight)
    summary <- rbind(summary, local_summary)
  }
    colnames(summary) <- c("Date", "Sector", "Weight" )
    View(summary)
}   


