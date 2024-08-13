#' volimpl 
#'
#' @description Calculation of implied volatilities across maturities.
#' 
#' @details
#' Implied volatility doesn't always end-up matching realized volatility,
#' so we want to see where and investigate why. Prices can, probabilistically
#' follow an alternate path.
#' 
#' And to become SWIFTLY familiar with what a VOL number might be trying to tell you about the current state of MKT.
#' Historical options data (incl. IV at the time): [CHECK]
#' Alternative scenarios for IV (get all possibilities, see below), why?
#' Euan Sinclair book can give us an idea of why, e.g. prevailing characteristics in market at time?
#' At that point in time, would you have been thinking about the option IV differently, why?
#'
#' @noRd



# 
# Map IVs to historical vols
#
# SPX / INX, UKX, YFS, XJO (where to go)


#' idx.vols.impl - implied volatility calculation
#'
#' @param idx.orats 
#' @param dividendYield 
#' @param interestRate 
#' @param vols 
#'
#' @return
#' @export
#'
#' @examples
idx.vols.impl <- function(idx.orats, dividendYield = NULL, interestRate = NULL, vols = NULL) {
  
  # 1. Check named arguments, if NULL then assume values from orats dataset
  if (is.null(dividendYield)) {
    dividendYield <- 0.02
    message(
      paste0("Dividend yield not supplied, an accepted long run value of ", dividendYield," is used."))
  }
  
  if (is.null(interestRate)) {
    interestRate <- 0.0422
    message(
      paste0("Risk free rate not supplied, an accepted long run value of ", interestRate," is used."))
  }
  
  if (is.null(vols)) {
    message(
      "Volatility (vols) not supplied, ORATs smvVol assumed. See ORATS for details: https://orats.com/docs/definitions"
    )
  }
  
  # 2. Rundate - when the instance of THIS calculation was run (incl. seconds)
  runDate <- as.POSIXct(Sys.time(), tz = "UTC")
  
  # 3. Maturity function
  # JT, 2024-06-14: Syntactic sugar on difftime, should be separate helper function.
  maturity <- function (expirydate, tradedate) {
    as.double(base::difftime(expirydate, tradedate, units = "days") / 252)
  }
  
  # 4. List maturities
  matList <- names(idx.orats)
  matList_len <- base::length(matList)
  
  # 5. EOIV
  # For each maturity return:
  #   - price
  #     - greeks
  #   - IV at strike
  
  for (i in matList_len) {
    ##########
    # Maturity
    ##########
    # 1. 
    
    mat <- names(idx.orats[30])
    
    ##########
    # Calls
    ##########
    # 1. Underlying spot price
    callStrikes <- as.data.frame(idx.orats[[30]][["extra"]][["spotPrice"]])
    colnames(callStrikes)[1] <- "underlying"
    
    # 2. Strike price
    callStrikes$strike <- idx.orats[[30]][["call"]][["Strike"]]
    
    # 3. Dividend yield
    callStrikes$dividendYield <- dividendYield
    
    # 4. Risk-free rate
    callStrikes$interestRate <- interestRate
    
    # 5. Maturity
    expiryDate <- base::as.Date(names(idx.orats[30]), format = "%b.%d.%Y") # from the data frames maybe?
    tradeDate <- base::as.Date(unique(idx.orats[[30]][["extra"]][["tradeDate"]]), format = "%Y.%m.%d")
    
    callStrikes$maturity <- maturity(expiryDate, tradeDate)
    
    # 6. Volatility
    if (is.null(vols)) {
      callStrikes$smvVol <- idx.orats[[30]][["extra"]][["smvVol"]]
    } else {
      callStrikes$vol <- vols
    }
    
    # Price
    callStrikes <- callStrikes %>% mutate(
      price = pmap_dbl(.,
                       ~ RQuantLib::EuropeanOption(
                         type = "call",
                         underlying = ..1,
                         strike = ..2,
                         dividendYield = ..3,
                         riskFreeRate = ..4,
                         maturity = ..5,
                         volatility = ..6)$value
      )
    )
    
    # Implied Volatility
    callStrikes <- callStrikes %>% mutate(
      implVol = pmap_dbl(.,
                         ~ RQuantLib::EuropeanOptionImpliedVolatility(
                           type = "call",
                           value = ..7,
                           underlying = ..1,
                           strike = ..2,
                           dividendYield = ..3,
                           riskFreeRate = ..4,
                           maturity = ..5,
                           volatility = 0.4)
      )
    )
    
    # Spot distance weighting factor
    a <- .25
    callStrikes$distanceFactor <- abs(1 - (callStrikes$strike / callStrikes$underlying))
    
    # Got here, needs a case_when to check this condition!! JT, 2024-06-14, 15:53.
    callStrikes$distanceWeight <- 
      dplyr::case_when(
        callStrikes$distanceFactor > a ~ 0,
        callStrikes$distanceFactor < a ~ ((callStrikes$distanceFactor - a)^2) / a^2)
    
    # Volume / Volume Weighting Factor
    callStrikes$volume <- idx.orats[[30]][["call"]][["Vol"]]
    callStrikes$volumeFactor <- callStrikes$volume / sum(callStrikes$volume)
    
    # Composite Implied Volatility
    c_compImpl_Num <- sum (callStrikes$volumeFactor * callStrikes$distanceWeight * callStrikes$implVol)
    c_compImpl_Den <- sum (callStrikes$volumeFactor * callStrikes$distanceWeight)
    callStrikes$compImplVol <- c_compImpl_Num / c_compImpl_Den
    
    ##########
    # Puts
    ##########
    # 1. Underlying spot price
    putStrikes <- as.data.frame(idx.orats[[30]][["extra"]][["spotPrice"]])
    colnames(putStrikes)[1] <- "underlying"
    
    # 2. Strike price
    putStrikes$strike <- idx.orats[[30]][["put"]][["Strike"]]
    
    # 3. Dividend yield
    putStrikes$dividendYield <- dividendYield
    
    # 4. Risk-free rate
    putStrikes$interestRate <- interestRate
    
    # 5. Maturity
    expiryDate <- base::as.Date(names(idx.orats[30]), format = "%b.%d.%Y") # from the data frames maybe?
    tradeDate <- base::as.Date(unique(idx.orats[[30]][["extra"]][["tradeDate"]]), format = "%Y.%m.%d")
    
    putStrikes$maturity <- maturity(expiryDate, tradeDate)
    
    # 6. Volatility
    if (is.null(vols)) {
      putStrikes$smvVol <- idx.orats[[30]][["extra"]][["smvVol"]]
    } else {
      putStrikes$vol <- vols
    }
    
    # Price
    putStrikes <- putStrikes %>% mutate(
      price = pmap_dbl(.,
                       ~ RQuantLib::EuropeanOption(
                         type = "put",
                         underlying = ..1,
                         strike = ..2,
                         dividendYield = ..3,
                         riskFreeRate = ..4,
                         maturity = ..5,
                         volatility = ..6)$value
      )
    )
    
    # Implied Volatility
    putStrikes <- putStrikes %>% mutate(
      implVol = pmap_dbl(.,
                         ~ RQuantLib::EuropeanOptionImpliedVolatility(
                           type = "put",
                           value = ..7,
                           underlying = ..1,
                           strike = ..2,
                           dividendYield = ..3,
                           riskFreeRate = ..4,
                           maturity = ..5,
                           volatility = 0.4)
      )
    )
    
    # Spot distance weighting factor
    putStrikes$distanceFactor <- abs(1 - (putStrikes$strike / putStrikes$underlying))
    
    # Got here, needs a case_when to check this condition!! JT, 2024-06-14, 15:53.
    putStrikes$distanceWeight <- 
      dplyr::case_when(
        putStrikes$distanceFactor > a ~ 0,
        putStrikes$distanceFactor < a ~ ((putStrikes$distanceFactor - a)^2) / a^2)
    
    # Volume / Volume Weighting Factor
    putStrikes$volume <- idx.orats[[30]][["put"]][["Vol"]]
    putStrikes$volumeFactor <- putStrikes$volume / sum(putStrikes$volume)
    
    # Composite Implied Volatility
    p_compImpl_Num <- sum (putStrikes$volumeFactor * putStrikes$distanceWeight * putStrikes$implVol)
    p_compImpl_Den <- sum (putStrikes$volumeFactor * putStrikes$distanceWeight)
    putStrikes$compImplVol <- p_compImpl_Num / p_compImpl_Den
    
    return(list(mat = mat, call = callStrikes, put = putStrikes))
    
    # mat = mat, - Got to here, 25/06/2024, 15:04.*****
  }
  
}
