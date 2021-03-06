#' MSCI Min Volatility data from 10/31/2011 to 01/05/2017
#' 
#' This data frame contains the MSCI Min Volatility indices and their consitutents from 
#' 2011-10-31 to 2017-01-05. 
#' 
#' @format A data frame with 11 variables 
#' \itemize{ 
#'   \item Ticker = The symbol of each security.
#'   \item Name = The name of each security.
#'   \item Asset Class = The asset class of each security.
#'   \item Weight = The weight of each asset relative to the entire portfolio
#'   \item Price = Spot price of asset 
#'   \item Shares = Number of shares of asset in portfolio
#'   \item Market Value = Market Value of asset at spot price
#'   \item Notional Value = Total amount of security's underlying asset value at spot price
#'   \item Sector = Sector of asset
#'   \item SEDOL = Stock Exchange Daily Official List Number
#'   \item ISIN = International Securities Identification Number
#'   \item Exchange = Stock Exchange where asset is traded
#'   \item Date = Date when asset was in the portfolio
#'   }
#' @docType data
#' @name minvol
#'
#' @keywords security asset min volatility msci data
NULL