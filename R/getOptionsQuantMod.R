require("quantmod")

SPX.OPTS <- getOptionChain("SPX", NULL, src = "orats", api.key = "e4cd15f5-4dd0-41d0-bb9e-bf27d79df0ff")

data <- do.call(rbind, lapply(SPX.OPTS, function(x) do.call(rbind, x)))

ukx <- getSymbols('UKX', src = "av", api.key = "0KREDSGZQV0QH8UM", auto.assign = FALSE)

GetReturn <- function(tickers){
  library(quantmod)
  
  data.env <- new.env()
  dataset<- xts() # Only run once
  
  
  # Download prices from AlphaVantage and calculate log-returns
  for(i in 1:length(tickers)) {
    tickers[i]-> symbol
    print(symbol)
    getSymbols(symbol, src="av",
               auto.assign=TRUE,
               output.size="full",
               adjusted=TRUE,
               api.key="0KREDSGZQV0QH8UM")
    
    dataset <- merge(dataset, periodReturn(Ad(get(tickers[i])),period="daily", type='log'))
    rm(symbol)
  }
  
  names(dataset)<-tickers
  
  return(dataset)
}

tickers<-c("^GSPC", "^FTSE", "^GDAXI", "^N100", "^BVSP")
dataset<-GetReturn(tickers)