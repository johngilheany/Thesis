rel_weight <- function(){
  
library(mscir)
library(dplyr)
data(usa)


usa1 <- usa[which(usa$Date =="2012-12-31"),]
sector_name <- unique(usa1$Sector)
sector_count <- table(usa1$Sector)
total <- sum(sector)
percent <- sector_count / total
date <- "2012-12-31"
class(date) <-"Date"
table1 <- cbind(sector_count, total, percent, date)
View(table1)

usa2 <- usa[which(usa$Date =="2011-10-31"), ]
sector_count <- table(usa2$Sector)
total <- sum(sector)
percent <- sector_count / total
date <- "2011-10-31"
class(date) <-"Date"
table2 <- cbind(sector_count, total, percent, date)
View(table2)

table3 <- rbind(table1, table2)
View(table3)

Telecom1 <- table3[which(table3 =="Telecommunication Services"), ]

