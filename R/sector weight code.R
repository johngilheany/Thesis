sector_weight <- function(sector){
  if (sector == "Financials"){
    data(usa_weight)
    data(minvol_weight)
    temp1 <- usa_weight[which(usa_weight$Sector=="Financials"), ]
    temp2 <- minvol_weight[which(minvol_weight$Sector=="Financials"), ]
    ggplot(temp1, aes(Date, Weight, colour = "USA")) + geom_line() + 
      ggtitle("USA vs. Min Vol Financial Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
      geom_line(data = temp2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
  } else {
    if (sector == "Energy"){
      data(usa_weight)
      data(minvol_weight)
      temp1 <- usa_weight[which(usa_weight$Sector=="Energy"), ]
      temp2 <- minvol_weight[which(minvol_weight$Sector=="Energy"), ]
      ggplot(temp1, aes(Date, Weight, colour = "USA")) + geom_line() + 
        ggtitle("USA vs. Min Vol Energy Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
        geom_line(data = temp2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
    } else {
      if (sector == "Consumer Staples"){
        data(usa_weight)
        data(minvol_weight)
        temp1 <- usa_weight[which(usa_weight$Sector=="Consumer Staples"), ]
        temp2 <- minvol_weight[which(minvol_weight$Sector=="Consumer Staples"), ]
        ggplot(temp1, aes(Date, Weight, colour = "USA")) + geom_line() + 
          ggtitle("USA vs. Min Vol Consumer Staples Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
          geom_line(data = temp2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
      } else {
        if (sector == "Consumer Discretionary"){
          data(usa_weight)
          data(minvol_weight)
          temp1 <- usa_weight[which(usa_weight$Sector=="Consumer Discretionary"), ]
          temp2 <- minvol_weight[which(minvol_weight$Sector=="Consumer Discretionary"), ]
          ggplot(temp1, aes(Date, Weight, colour = "USA")) + geom_line() + 
            ggtitle("USA vs. Min Vol Consumer Discretionary Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
            geom_line(data = temp2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
        } else {
          if (sector == "Health Care"){
            data(usa_weight)
            data(minvol_weight)
            temp1 <- usa_weight[which(usa_weight$Sector=="Health Care"), ]
            temp2 <- minvol_weight[which(minvol_weight$Sector=="Health Care"), ]
            ggplot(temp1, aes(Date, Weight, colour = "USA")) + geom_line() + 
              ggtitle("USA vs. Min Vol Health Care Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
              geom_line(data = temp2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
          } else {
            if (sector == "Industrials"){
              data(usa_weight)
              data(minvol_weight)
              temp1 <- usa_weight[which(usa_weight$Sector=="Industrials"), ]
              temp2 <- minvol_weight[which(minvol_weight$Sector=="Industrials"), ]
              ggplot(temp1, aes(Date, Weight, colour = "USA")) + geom_line() + 
                ggtitle("USA vs. Min Vol Industrials Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
                geom_line(data = temp2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
            } else {
              if (sector == "Information Technology"){
                data(usa_weight)
                data(minvol_weight)
                temp1 <- usa_weight[which(usa_weight$Sector=="Information Technology"), ]
                temp2 <- minvol_weight[which(minvol_weight$Sector=="Information Technology"), ]
                ggplot(temp1, aes(Date, Weight, colour = "USA")) + geom_line() + 
                  ggtitle("USA vs. Min Vol Information Technology Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
                  geom_line(data = temp2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
              } else {
                if (sector == "Materials"){
                  data(usa_weight)
                  data(minvol_weight)
                  temp1 <- usa_weight[which(usa_weight$Sector=="Materials"), ]
                  temp2 <- minvol_weight[which(minvol_weight$Sector=="Materials"), ]
                  ggplot(temp1, aes(Date, Weight, colour = "USA")) + geom_line() + 
                    ggtitle("USA vs. Min Vol Materials Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
                    geom_line(data = temp2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
                } else {
                  if (sector == "S-T Securities"){
                    data(usa_weight)
                    data(minvol_weight)
                    temp1 <- usa_weight[which(usa_weight$Sector=="S-T Securities"), ]
                    temp2 <- minvol_weight[which(minvol_weight$Sector=="S-T Securities"), ]
                    ggplot(temp1, aes(Date, Weight, colour = "USA")) + geom_line() + 
                      ggtitle("USA vs. Min Vol S-T Securities Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
                      geom_line(data = temp2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
                  } else {
                    if (sector == "Telecommunications Services"){
                      data(usa_weight)
                      data(minvol_weight)
                      temp1 <- usa_weight[which(usa_weight$Sector=="Telecommunications Services"), ]
                      temp2 <- minvol_weight[which(minvol_weight$Sector=="Telecommunications Services"), ]
                      ggplot(temp1, aes(Date, Weight, colour = "USA")) + geom_line() + 
                        ggtitle("USA vs. Min Vol Telecommunications Services Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
                        geom_line(data = temp2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
                    } else {
                      if (sector == "Utilities"){
                        data(usa_weight)
                        data(minvol_weight)
                        temp1 <- usa_weight[which(usa_weight$Sector=="Utilities"), ]
                        temp2 <- minvol_weight[which(minvol_weight$Sector=="Utilities"), ]
                        ggplot(temp1, aes(Date, Weight, colour = "USA")) + geom_line() + 
                          ggtitle("USA vs. Min Vol Utilities Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
                          geom_line(data = temp2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
                      } else {
                        if (sector == "All"){
                          data(usa_weight)
                          data(minvol_weight)
                          
                          Eng1 <- usa_weight[which(usa_weight$Sector=="Energy"), ]
                          Eng2 <- minvol_weight[which(minvol_weight$Sector=="Energy"), ]
                          plot1 <- ggplot(Eng1, aes(Date, Weight, colour = "USA")) + geom_line() + 
                            ggtitle("USA vs. Min Vol Energy Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
                            geom_line(data = Eng2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
                          
                          Fin1 <- usa_weight[which(usa_weight$Sector=="Financials"), ]
                          Fin2 <- minvol_weight[which(minvol_weight$Sector=="Financials"), ]
                          plot2 <- ggplot(Fin1, aes(Date, Weight, colour = "USA")) + geom_line() + 
                            ggtitle("USA vs. Min Vol Financial Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
                            geom_line(data = Fin2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
                          
                          ConStap1 <- usa_weight[which(usa_weight$Sector=="Consumer Staples"), ]
                          ConStap2 <- minvol_weight[which(minvol_weight$Sector=="Consumer Staples"), ]
                          plot3 <- ggplot(ConStap1, aes(Date, Weight, colour = "USA")) + geom_line() + 
                            ggtitle("USA vs. Min Vol Consumer Staples Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
                            geom_line(data = ConStap2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
                          
                          ConDis1 <- usa_weight[which(usa_weight$Sector=="Consumer Discretionary"), ]
                          ConDis2 <- minvol_weight[which(minvol_weight$Sector=="Consumer Discretionary"), ]
                          plot4 <- ggplot(ConDis1, aes(Date, Weight, colour = "USA")) + geom_line() + 
                            ggtitle("USA vs. Min Vol Consumer Discretionary Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
                            geom_line(data = ConDis2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
                          
                          Health1 <- usa_weight[which(usa_weight$Sector=="Health Care"), ]
                          Health2 <- minvol_weight[which(minvol_weight$Sector=="Health Care"), ]
                          plot5 <- ggplot(Health1, aes(Date, Weight, colour = "USA")) + geom_line() + 
                            ggtitle("USA vs. Min Vol Health Care Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
                            geom_line(data = Health2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
                          
                          Ind1 <- usa_weight[which(usa_weight$Sector=="Industrials"), ]
                          Ind2 <- minvol_weight[which(minvol_weight$Sector=="Industrials"), ]
                          plot6 <- ggplot(Ind1, aes(Date, Weight, colour = "USA")) + geom_line() + 
                            ggtitle("USA vs. Min Vol Industrials Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
                            geom_line(data = Ind2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
                          
                          IT1 <- usa_weight[which(usa_weight$Sector=="Information Technology"), ]
                          IT2 <- minvol_weight[which(minvol_weight$Sector=="Information Technology"), ]
                          plot7 <- ggplot(IT1, aes(Date, Weight, colour = "USA")) + geom_line() + 
                            ggtitle("USA vs. Min Vol Information Technology Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
                            geom_line(data = IT2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
                          
                          Mat1 <- usa_weight[which(usa_weight$Sector=="Materials"), ]
                          Mat2 <- minvol_weight[which(minvol_weight$Sector=="Materials"), ]
                          plot8 <- ggplot(Mat1, aes(Date, Weight, colour = "USA")) + geom_line() + 
                            ggtitle("USA vs. Min Vol Materials Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
                            geom_line(data = Mat2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
                          
                          ST1 <- usa_weight[which(usa_weight$Sector=="S-T Securities"), ]
                          ST2 <- minvol_weight[which(minvol_weight$Sector=="S-T Securities"), ]
                          plot9 <- ggplot(ST1, aes(Date, Weight, colour = "USA")) + geom_line() + 
                            ggtitle("USA vs. Min Vol S-T Securities Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
                            geom_line(data = ST2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
                          
                          Telecom1 <- usa_weight[which(usa_weight$Sector=="Telecommunications Services"), ]
                          Telecom2 <- minvol_weight[which(minvol_weight$Sector=="Telecommunications Services"), ]
                          plot10 <- ggplot(Telecom1, aes(Date, Weight, colour = "USA")) + geom_line() + 
                            ggtitle("USA vs. Min Vol Telecommunications Services Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
                            geom_line(data = Telecom2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
                          
                          Util1 <- usa_weight[which(usa_weight$Sector=="Utilities"), ]
                          Util2 <- minvol_weight[which(minvol_weight$Sector=="Utilities"), ]
                          plot11 <- ggplot(Util1, aes(Date, Weight, colour = "USA")) + geom_line() + 
                            ggtitle("USA vs. Min Vol Utilities Sector Weights") + xlab("Time") + ylab("Sector Weight") + 
                            geom_line(data = Util2, aes(x=Date, y=Weight, colour="Min Vol"),show.legend = TRUE)
                          
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

  
  