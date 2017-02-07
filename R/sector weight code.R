#Financials
## Min Vol Financials Data Subset
minvol_weight_financials <- minvol_weight[which(minvol_weight$Sector=="Financials"), ]
## Min Vol Financials Time Series
ggplot(minvol_weight_financials, aes(Date, Weight)) + geom_line() + ggtitle("Min Vol Financial Sector Weights") + xlab("Time") + ylab("Sector Weight") 
### USA Financials Data Subset
usa_weight_financials <- usa_weight[which(usa_weight$Sector=="Financials"), ]
### USA Financials Time Series
ggplot(usa_weight_financials, aes(Date, Weight)) + geom_line() + ggtitle("USA Financial Sector Weights") + xlab("Time") + ylab("Sector Weight") 
#### Overlay of Min Vol and USA Financials Time Series
ggplot(usa_weight_financials, aes(Date, Weight, colour = "USA")) + 
  geom_line() + ggtitle("USA vs. Min Vol Financial Sector Weights") + 
  xlab("Time") + ylab("Sector Weight") +
  geom_line(data = minvol_weight_financials, 
  aes(x=Date, y=Weight, colour="MinVol"),show.legend = TRUE)

minvol_sector_weight <- function(sector){
  if (sector == "Financials"){
    data(minvol_weight)
    temp <- minvol_weight[which(minvol_weight$Sector=="Financials"), ]
    ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("Min Vol Financial Sector Weight") + xlab("Time") + ylab("Sector Weight") 
  } else {
    if (sector == "Energy"){
      data(minvol_weight)
      temp <- minvol_weight[which(minvol_weight$Sector=="Energy"), ]
      ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("Min Vol Energy Sector Weight") + xlab("Time") + ylab("Sector Weight") 
    } else {
      if (sector == "Consumer Staples"){
        data(minvol_weight)
        temp <- minvol_weight[which(minvol_weight$Sector=="Consumer Staples"), ]
        ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("Min Vol Consumer Staples Sector Weight") + xlab("Time") + ylab("Sector Weight") 
      } else {
        if (sector == "Consumer Discretionary"){
          data(minvol_weight)
          temp <- minvol_weight[which(minvol_weight$Sector=="Consumer Discretionary"), ]
          ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("Min Vol Consumer Discretionary Sector Weight") + xlab("Time") + ylab("Sector Weight") 
        } else {
          if (sector == "Health Care"){
            data(minvol_weight)
            temp <- minvol_weight[which(minvol_weight$Sector=="Health Care"), ]
            ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("Min Vol Health Care Sector Weight") + xlab("Time") + ylab("Sector Weight") 
          } else {
            if (sector == "Industrials"){
              data(minvol_weight)
              temp <- minvol_weight[which(minvol_weight$Sector=="Industrials"), ]
              ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("Min Vol Industrials Sector Weight") + xlab("Time") + ylab("Sector Weight") 
            } else {
              if (sector == "Information Technology"){
                data(minvol_weight)
                temp <- minvol_weight[which(minvol_weight$Sector=="Information Technology"), ]
                ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("Min Vol Information Technology Sector Weight") + xlab("Time") + ylab("Sector Weight") 
              } else {
                if (sector == "Materials"){
                  data(minvol_weight)
                  temp <- minvol_weight[which(minvol_weight$Sector=="Materials"), ]
                  ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("Min Vol Materials Sector Weight") + xlab("Time") + ylab("Sector Weight") 
                } else {
                  if (sector == "S-T Securities"){
                    data(minvol_weight)
                    temp <- minvol_weight[which(minvol_weight$Sector=="S-T Securities"), ]
                    ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("Min Vol S-T Securities Weight") + xlab("Time") + ylab("Sector Weight") 
                  } else {
                    if (sector == "Telecommunication Services"){
                      data(minvol_weight)
                      temp <- minvol_weight[which(minvol_weight$Sector=="Telecommunication Services"), ]
                      ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("Min Vol Telecommunication Services Weight") + xlab("Time") + ylab("Sector Weight") 
                    } else {
                      if (sector == "Utilities"){
                        data(minvol_weight)
                        temp <- minvol_weight[which(minvol_weight$Sector=="Utilities"), ]
                        ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("Min Vol Utilities Weight") + xlab("Time") + ylab("Sector Weight") 
                      } else {
                        if (sector == "All"){
                          data(minvol_weight)
                          ggplot() +
                          geom_line (data = minvol_weight[which(minvol_weight$Sector=="Financials"), ], aes(Date, Weight, color = "Financials")) +
                          geom_line (data = minvol_weight[which(minvol_weight$Sector=="Energy"), ], aes(Date, Weight, color = "Energy")) +
                          geom_line (data = minvol_weight[which(minvol_weight$Sector=="Consumer Staples"), ], aes(Date, Weight, color = "Consumer Staples")) +
                          geom_line (data = minvol_weight[which(minvol_weight$Sector=="Consumer Discretionary"), ], aes(Date, Weight, color = "Consumer Discretionary")) +
                          geom_line (data = minvol_weight[which(minvol_weight$Sector=="Health Care"), ], aes(Date, Weight, color = "Health Care")) +
                          geom_line (data = minvol_weight[which(minvol_weight$Sector=="Industrials"), ], aes(Date, Weight, color = "Industrials")) +
                          geom_line (data = minvol_weight[which(minvol_weight$Sector=="Information Technology"), ], aes(Date, Weight, color = "Information Technology")) +
                          geom_line (data = minvol_weight[which(minvol_weight$Sector=="Materials"), ], aes(Date, Weight, color = "Materials")) + 
                          geom_line (data = minvol_weight[which(minvol_weight$Sector=="S-T Securities"), ], aes(Date, Weight, color = "S-T Securities")) +
                          geom_line (data = minvol_weight[which(minvol_weight$Sector=="Telecommunication Services"), ], aes(Date, Weight, color = "Telecommunication Services")) +
                          geom_line (data = minvol_weight[which(minvol_weight$Sector=="Utilities"), ], aes(Date, Weight, color = "Utilities")) +
                          ggtitle("Min Vol Sector Weights") + xlab('Time') + ylab('Sector Weight')
                        }
                      } 
                      
                      
                        }
                      }
                      }   
                  }
                }
              }
            }
          }
        }    
  }
  }
  

  
  
  
  