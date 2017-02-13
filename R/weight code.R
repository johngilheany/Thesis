#Create vector with unique dates
data(usa)
dates <- as.Date(unique(usa$Date))
library("zoo")

summary <- data.frame();

for (Date in dates){
  # Extract entries with the matched date
  usa_by_date <- usa[which(temp$Date == date),]
  sector <- table(usa_by_date$Sector)
  total <- sum(sector)
  usa_sector <- cbind (sector, total)
  percent <- sector / total
  date <- as.Date(date, origin = "1970-01-01")
  class(date) <-"Date"
  usa_sector_percent <- cbind(sector, total, percent, date)
  summary_usa <- rbind(summary, usa_sector_percent)
}








