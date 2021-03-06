library(mscir)
library(tidyquant)
library(dplyr)
data(minvol)

minvol_percent <- data.frame();

minvol1 <- minvol[which(minvol$Date =="2011-10-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2011-10-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2011-11-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2011-11-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2011-12-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2011-12-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2012-01-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2012-01-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2012-02-29"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2012-02-29"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2012-03-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2012-03-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2012-04-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2012-04-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2012-05-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2012-05-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2012-06-29"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2012-06-29"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2012-07-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2012-07-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2012-08-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2012-08-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2012-09-28"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2012-09-28"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2012-10-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2012-10-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2012-11-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2012-11-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2012-12-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2012-12-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2013-01-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2013-01-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2013-02-28"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2013-02-28"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2013-03-28"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2013-03-28"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2013-04-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2013-04-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2013-05-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2013-05-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2013-06-28"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2013-06-28"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2013-07-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2013-07-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2013-08-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2013-08-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2013-09-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2013-09-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2013-10-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2013-10-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2013-11-29"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2013-11-29"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2013-12-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2013-12-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2014-01-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2014-01-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2014-02-28"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2014-02-28"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2014-03-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2014-03-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2014-04-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2014-04-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2014-05-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2014-05-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2014-06-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2014-06-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2014-07-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2014-07-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2014-08-29"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2014-08-29"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2014-09-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2014-09-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2014-10-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2014-10-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2014-11-28"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2014-11-28"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2014-12-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2014-12-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2015-01-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2015-01-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2015-02-27"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2015-02-27"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2015-03-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2015-03-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2015-04-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2015-04-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2015-05-29"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2015-05-29"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2015-06-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2015-06-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2015-07-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2015-07-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2015-08-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2015-08-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2015-09-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2015-09-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2015-10-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2015-10-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2015-11-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2015-11-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2015-12-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2015-12-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2016-01-29"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2016-01-29"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2016-02-29"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2016-02-29"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2016-03-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2016-03-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2016-04-29"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2016-04-29"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2016-05-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2016-05-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2016-06-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2016-06-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2016-07-29"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2016-07-29"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2016-08-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2016-08-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2016-09-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2016-09-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2016-10-31"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2016-10-31"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2016-11-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2016-11-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2016-12-30"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2016-12-30"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

minvol1 <- minvol[which(minvol$Date =="2017-01-05"),]
sector_name <- unique(minvol1$Sector)
sector_count <- table(minvol1$Sector)
total <- nrow(minvol1)
percent <- sector_count / total
date <- "2017-01-05"
table1 <- cbind(sector_count, total, percent, date)
table1 <- as_tibble(table1, preserve_row_names = TRUE)
table1 <- table1 %>%
  mutate(date = lubridate::ymd(date))
colnames(table1)[1] <- "sector"
table1 <- transform(table1, percent = as.numeric(percent))
minvol_percent <- rbind(table1, minvol_percent)

View(minvol_percent)
