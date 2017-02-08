usa_sector_weight <- function(sector){
  if (sector == "Financials"){
    data(usa_weight)
    temp <- usa_weight[which(usa_weight$Sector=="Financials"), ]
    ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("USA Financial Sector Weight") + xlab("Time") + ylab("Sector Weight") 
  } else {
    if (sector == "Energy"){
      data(usa_weight)
      temp <- usa_weight[which(usa_weight$Sector=="Energy"), ]
      ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("USA Energy Sector Weight") + xlab("Time") + ylab("Sector Weight") 
    } else {
      if (sector == "Consumer Staples"){
        data(usa_weight)
        temp <- usa_weight[which(usa_weight$Sector=="Consumer Staples"), ]
        ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("USA Consumer Staples Sector Weight") + xlab("Time") + ylab("Sector Weight") 
      } else {
        if (sector == "Consumer Discretionary"){
          data(usa_weight)
          temp <- usa_weight[which(usa_weight$Sector=="Consumer Discretionary"), ]
          ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("USA Consumer Discretionary Sector Weight") + xlab("Time") + ylab("Sector Weight") 
        } else {
          if (sector == "Health Care"){
            data(usa_weight)
            temp <- usa_weight[which(usa_weight$Sector=="Health Care"), ]
            ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("USA Health Care Sector Weight") + xlab("Time") + ylab("Sector Weight") 
          } else {
            if (sector == "Industrials"){
              data(usa_weight)
              temp <- usa_weight[which(usa_weight$Sector=="Industrials"), ]
              ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("USA Industrials Sector Weight") + xlab("Time") + ylab("Sector Weight") 
            } else {
              if (sector == "Information Technology"){
                data(usa_weight)
                temp <- usa_weight[which(usa_weight$Sector=="Information Technology"), ]
                ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("USA Information Technology Sector Weight") + xlab("Time") + ylab("Sector Weight") 
              } else {
                if (sector == "Materials"){
                  data(usa_weight)
                  temp <- usa_weight[which(usa_weight$Sector=="Materials"), ]
                  ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("USA Materials Sector Weight") + xlab("Time") + ylab("Sector Weight") 
                } else {
                  if (sector == "S-T Securities"){
                    data(usa_weight)
                    temp <- usa_weight[which(usa_weight$Sector=="S-T Securities"), ]
                    ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("USA S-T Securities Weight") + xlab("Time") + ylab("Sector Weight") 
                  } else {
                    if (sector == "Telecommunication Services"){
                      data(usa_weight)
                      temp <- usa_weight[which(usa_weight$Sector=="Telecommunication Services"), ]
                      ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("USA Telecommunication Services Weight") + xlab("Time") + ylab("Sector Weight") 
                    } else {
                      if (sector == "Utilities"){
                        data(usa_weight)
                        temp <- usa_weight[which(usa_weight$Sector=="Utilities"), ]
                        ggplot(temp, aes(Date, Weight)) + geom_line() + ggtitle("USA Utilities Weight") + xlab("Time") + ylab("Sector Weight") 
                      } else {
                        if (sector == "Overlay"){
                          data(usa_weight)
                          ggplot() +
                            geom_line (data = usa_weight[which(usa_weight$Sector=="Financials"), ], aes(Date, Weight, color = "Financials")) +
                            geom_line (data = usa_weight[which(usa_weight$Sector=="Energy"), ], aes(Date, Weight, color = "Energy")) +
                            geom_line (data = usa_weight[which(usa_weight$Sector=="Consumer Staples"), ], aes(Date, Weight, color = "Consumer Staples")) +
                            geom_line (data = usa_weight[which(usa_weight$Sector=="Consumer Discretionary"), ], aes(Date, Weight, color = "Consumer Discretionary")) +
                            geom_line (data = usa_weight[which(usa_weight$Sector=="Health Care"), ], aes(Date, Weight, color = "Health Care")) +
                            geom_line (data = usa_weight[which(usa_weight$Sector=="Industrials"), ], aes(Date, Weight, color = "Industrials")) +
                            geom_line (data = usa_weight[which(usa_weight$Sector=="Information Technology"), ], aes(Date, Weight, color = "Information Technology")) +
                            geom_line (data = usa_weight[which(usa_weight$Sector=="Materials"), ], aes(Date, Weight, color = "Materials")) + 
                            geom_line (data = usa_weight[which(usa_weight$Sector=="S-T Securities"), ], aes(Date, Weight, color = "S-T Securities")) +
                            geom_line (data = usa_weight[which(usa_weight$Sector=="Telecommunication Services"), ], aes(Date, Weight, color = "Telecommunication Services")) +
                            geom_line (data = usa_weight[which(usa_weight$Sector=="Utilities"), ], aes(Date, Weight, color = "Utilities")) +
                            ggtitle("USA Sector Weights") + xlab('Time') + ylab('Sector Weight')
                        } else {
                          if (sector == "All"){
                            data(usa_weight)
                            
                            temp1 <- usa_weight[which(usa_weight$Sector=="Financials"), ]
                            plot1 <- ggplot(temp1, aes(Date, Weight)) + geom_line() + ggtitle("USA Financial Sector Weight") + xlab("Time") + ylab("Sector Weight") 
                            
                            temp2 <- usa_weight[which(usa_weight$Sector=="Energy"), ]
                            plot2 <- ggplot(temp2, aes(Date, Weight)) + geom_line() + ggtitle("USA Energy Sector Weight") + xlab("Time") + ylab("Sector Weight") 
                            
                            temp3 <- usa_weight[which(usa_weight$Sector=="Consumer Staples"), ]
                            plot3 <- ggplot(temp3, aes(Date, Weight)) + geom_line() + ggtitle("USA Consumer Staples Sector Weight") + xlab("Time") + ylab("Sector Weight") 
                            
                            temp4 <- usa_weight[which(usa_weight$Sector=="Consumer Discretionary"), ]
                            plot4 <- ggplot(temp4, aes(Date, Weight)) + geom_line() + ggtitle("USA Consumer Discretionary Sector Weight") + xlab("Time") + ylab("Sector Weight") 
                            
                            temp5 <- usa_weight[which(usa_weight$Sector=="Health Care"), ]
                            plot5 <- ggplot(temp5, aes(Date, Weight)) + geom_line() + ggtitle("USA Health Care Sector Weight") + xlab("Time") + ylab("Sector Weight") 
                            
                            temp6 <- usa_weight[which(usa_weight$Sector=="Industrials"), ]
                            plot6 <- ggplot(temp6, aes(Date, Weight)) + geom_line() + ggtitle("USA Industrials Sector Weight") + xlab("Time") + ylab("Sector Weight") 
                            
                            temp7 <- usa_weight[which(usa_weight$Sector=="Information Technology"), ]
                            plot7 <- ggplot(temp7, aes(Date, Weight)) + geom_line() + ggtitle("USA Information Technology Sector Weight") + xlab("Time") + ylab("Sector Weight") 
                            
                            temp8 <- usa_weight[which(usa_weight$Sector=="Materials"), ]
                            plot8 <- ggplot(temp8, aes(Date, Weight)) + geom_line() + ggtitle("USA Materials Sector Weight") + xlab("Time") + ylab("Sector Weight") 
                            
                            temp9 <- usa_weight[which(usa_weight$Sector=="S-T Securities"), ]
                            plot9 <- ggplot(temp9, aes(Date, Weight)) + geom_line() + ggtitle("USA S-T Securities Weight") + xlab("Time") + ylab("Sector Weight") 
                            
                            temp10 <- usa_weight[which(usa_weight$Sector=="Telecommunication Services"), ]
                            plot10 <- ggplot(temp10, aes(Date, Weight)) + geom_line() + ggtitle("USA Telecommunication Services Weight") + xlab("Time") + ylab("Sector Weight") 
                            
                            temp11 <- usa_weight[which(usa_weight$Sector=="Utilities"), ]
                            plot11 <- ggplot(temp11, aes(Date, Weight)) + geom_line() + ggtitle("USA Utilities Weight") + xlab("Time") + ylab("Sector Weight") 
                            
                            #multiplot function (Winston Chang's Cookbook for R)
                            multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
                              require(grid)
                              plots <- c(list(...), plotlist)
                              numPlots = length(plots)
                              if (is.null(layout)) {
                                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                                 ncol = cols, nrow = ceiling(numPlots/cols))
                              }
                              if (numPlots == 1) {
                                print(plots[[1]])
                              } else {
                                grid.newpage()
                                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                                for (i in 1:numPlots) {
                                  matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                                  print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                                  layout.pos.col = matchidx$col))
                                }
                              }
                            }
                            multiplot(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10, plot11, cols=3)
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
}
