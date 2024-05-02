# Implied volatilities
# Map IVs to historical vols
idx.vols.impl <- function(idx.orats, dividendYield, interestRate, volatilities) {
  
  # Trade date (for integrity to the calculation the from the idx.orats source)
  tradeDate <- base::as.Date(unique(idx.orats[[1]][["extra"]][["tradeDate"]]), format = "%Y.%m.%d")
  
  # Expiry dates
  expiryDates <- base::as.Date(names(idx.orats), format = "%b.%d.%Y")
  
  # Maturity function
  maturity <- function (expirydate, tradedate) {
    base::difftime(expirydate, tradedate, units = "days") / 252
  }
  
  # Maturities calculation
  maturities <- purrr::map_dbl(expiryDates, maturity, tradeDate)
  
  # Calls
  calls <- RQuantLib::EuropeanOptionArrays("call", spotPrice, strikePrices, dividendYield, interestRate)
  
  # Puts
  puts <- RQuantLib::EuropeanOptionArrays("put", spotPrice, strikePrices, dividendYield, interestRate)
  
  
  
  # return(maturities)
}

# orats smvVol, RQuantLib bsm IV (using which est. vol?), diff?

EO_C <- EuropeanOption("call", 5234.18, 5250, 0.02, 0.0422, 0.377, 0.1366)
EO_P <- EuropeanOption("put", 5234.18, 5250, 0.02, 0.0422, 0.377, 0.1366)

EO_C_2 <- EuropeanOption("call", 5234.18, 5200, 0.02, 0.0422, 0.377, 0.1366)
EO_P_2 <- EuropeanOption("put", 5234.18, 5200, 0.02, 0.0422, 0.377, 0.1366)

EO_C_IV <- EuropeanOptionImpliedVolatility("call", value=EO_C$value+0.50, 5234.18, 5250, 0.02, 0.0422, 0.377, 0.1366)
EO_P_IV <- EuropeanOptionImpliedVolatility("put", value=EO_P$value+0.50, 5234.18, 5250, 0.02, 0.0422, 0.377, 0.1366)

EO_C_2_IV <- EuropeanOptionImpliedVolatility("call", value=EO_C_2$value+0.50, 5234.18, 5200, 0.02, 0.0422, 0.377, 0.1366)
EO_P_2_IV <- EuropeanOptionImpliedVolatility("put", value=EO_P_2$value+0.50, 5234.18, 5200, 0.02, 0.0422, 0.377, 0.1366)

# SPX / INX, UKX, YFS, XJO (where to go)