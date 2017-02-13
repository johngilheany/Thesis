rel_weight <- function(){
  
library(mscir)
library(dplyr)
data(usa)

if usa[which(usa$Date =="2012-12-31"),] {
  sector <- table(usa_by_date$Sector)
  total <- sum(sector)
  percent <- sector / total
  date <- "2012-12-31"
  class(date) <-"Date"
  table1 <- cbind(sector, total, percent, date)
}
if usa[which(usa$Date =="2011-10-31"), ] {
  sector <- table(usa_by_date$Sector)
  total <- sum(sector)
  percent <- sector / total
  date <- "2011-10-31"
  class(date) <-"Date"
  table2 <- cbind(sector, total, percent, date)
 
usa1 <- usa[which(usa$Date =="2012-12-31"),]
sector_name <- unique(usa1$Sector)
sector_count <- table(usa1$Sector)
total <- sum(sector)
percent <- sector_count / total
date <- "2012-12-31"
class(date) <-"Date"
table1 <- full_join(sector_count, total, percent, date)
View(table1)

usa2 <- usa[which(usa$Date =="2011-10-31"), ]
sector_count <- table(usa2$Sector)
total <- sum(sector)
percent <- sector_count / total
date <- "2011-10-31"
class(date) <-"Date"
table2 <- full_join(sector_count, total, percent, date)
View(table2)

table3 <- full_join(table1, table2)
View(table3)

Telecom1 <- table3[which(table3 =="Telecommunication Services"), ]

ggplot(Telecom1, aes(Date, Weight, colour = "USA")) + geom_line() + 
  ggtitle("USA vs. Min Vol Telecommunications Services Sector Weights") + 
  xlab("Time") + ylab("Sector Weight"))
