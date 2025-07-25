#' Calculate Implied Volatility using Black-Scholes
#'
#' @param S Current stock price
#' @param K Strike price
#' @param T Time to expiration (in years)
#' @param r Risk-free rate (default 0.02)
#' @param option_price Option price (bid or ask)
#' @param option_type "call" or "put"
#' @return Implied volatility
#' @keywords internal
calculate_implied_volatility <- function(S, K, T, r = 0.02, option_price, option_type = "call") {
  # Black-Scholes function
  black_scholes <- function(S, K, T, r, sigma, option_type) {
    d1 <- (log(S/K) + (r + sigma^2/2)*T) / (sigma*sqrt(T))
    d2 <- d1 - sigma*sqrt(T)
    
    if (option_type == "call") {
      price <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
    } else {
      price <- K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
    }
    return(price)
  }
  
  # Objective function for optimization
  objective <- function(sigma) {
    return(abs(black_scholes(S, K, T, r, sigma, option_type) - option_price))
  }
  
  # Use optimize to find implied volatility
  tryCatch({
    result <- optimize(objective, interval = c(0.01, 2.0))
    return(result$minimum)
  }, error = function(e) {
    return(0.20)  # Default IV if calculation fails
  })
}

#' Create Vertical Spreads from Options Data
#'
#' This function creates vertical spreads from options data based on specified parameters.
#' It supports multiple types of vertical spreads (long call, short call, long put, short put)
#' and can handle multiple days to expiry and strike price ranges.
#'
#' @param optionsData A data frame containing options data with required columns:
#'   - ticker: character
#'   - tradeDate: character (YYYY-MM-DD format)
#'   - expirDate: character (YYYY-MM-DD format)
#'   - dte: integer
#'   - strike: integer
#'   - stockPrice: numeric
#'   - spotPrice: numeric
#'   - callBidPrice: numeric
#'   - callAskPrice: numeric
#'   - putBidPrice: numeric
#'   - putAskPrice: numeric
#' @param dte Integer or vector of integers specifying days to expiry
#' @param dist Integer or vector of integers specifying the exact range between strike prices
#' @param callPos Integer or character specifying the column position/name for call prices
#' @param putPos Integer or character specifying the column position/name for put prices
#' @param vsType Character vector specifying vertical spread type(s): "lcs", "scs", "lps", "sps"
#' @param output_file Optional path to save results as CSV
#' @return A named list containing:
#'   - optionsData: Original input data frame
#'   - vsData: Data frame containing vertical spread information
#' @export
create_vertical_spreads <- function(optionsData, dte, dist, callPos, putPos, vsType, output_file = NULL) {
  # Input validation
  if (!is.data.frame(optionsData)) {
    stop("optionsData must be a data frame")
  }
  
  # Validate required columns
  required_cols <- c("ticker", "tradeDate", "expirDate", "dte", "strike", 
                    "stockPrice", "spotPrice", "callBidPrice", "callAskPrice",
                    "putBidPrice", "putAskPrice")
  if (!all(required_cols %in% names(optionsData))) {
    stop("Missing required columns in optionsData")
  }
  
  # Validate data types
  if (!is.character(optionsData$ticker)) stop("ticker must be character")
  if (!is.character(optionsData$tradeDate)) stop("tradeDate must be character")
  if (!is.character(optionsData$expirDate)) stop("expirDate must be character")
  if (!is.integer(optionsData$dte)) stop("dte must be integer")
  if (!is.numeric(optionsData$strike)) stop("strike must be numeric")
  if (!is.numeric(optionsData$stockPrice)) stop("stockPrice must be numeric")
  if (!is.numeric(optionsData$spotPrice)) stop("spotPrice must be numeric")
  
  # Validate numeric fields are non-negative
  numeric_cols <- c("strike", "stockPrice", "spotPrice", "callBidPrice", 
                   "callAskPrice", "putBidPrice", "putAskPrice")
  for (col in numeric_cols) {
    if (any(optionsData[[col]] < 0, na.rm = TRUE)) {
      stop(paste(col, "must be non-negative"))
    }
  }
  
  # Validate bid-ask relationships
  if (any(optionsData$callBidPrice > optionsData$callAskPrice, na.rm = TRUE)) {
    stop("callBidPrice must be less than or equal to callAskPrice")
  }
  if (any(optionsData$putBidPrice > optionsData$putAskPrice, na.rm = TRUE)) {
    stop("putBidPrice must be less than or equal to putAskPrice")
  }
  
  # Validate vsType
  valid_types <- c("lcs", "scs", "lps", "sps")
  if (!all(vsType %in% valid_types)) {
    stop("vsType must be one of: 'lcs', 'scs', 'lps', 'sps'")
  }
  
  # Convert column positions to names if needed
  if (is.numeric(callPos)) callPos <- names(optionsData)[callPos]
  if (is.numeric(putPos)) putPos <- names(optionsData)[putPos]
  
  # Validate price columns
  if (!all(c(callPos, putPos) %in% names(optionsData))) {
    stop("Specified price columns not found in optionsData")
  }
  
  if (!all(sapply(optionsData[, c(callPos, putPos)], is.numeric))) {
    stop("Price columns must contain numeric values")
  }
  
  # Initialize empty result data frame
  result <- data.frame()
  
  # Process each dte value
  for (days in dte) {
    # Filter data for current dte
    dte_data <- optionsData[optionsData$dte == days, ]
    
    if (nrow(dte_data) < 2) {
      warning(sprintf("Not enough options for dte %d", days))
      next
    }
    
    # Process each dist value for current dte
    for (dist_val in dist) {
      # Find valid strike pairs for current dist
      for (i in 1:(nrow(dte_data)-1)) {
        for (j in (i+1):nrow(dte_data)) {
          strike_diff <- abs(dte_data$strike[j] - dte_data$strike[i])
          if (strike_diff == dist_val) {
            # Mark strike types
            strikeL <- min(dte_data$strike[i], dte_data$strike[j])
            strikeH <- max(dte_data$strike[i], dte_data$strike[j])
            
            # Create new row for each spread type
            for (type in vsType) {
              # Determine instrument type and position type
              instrType <- ifelse(type %in% c("lcs", "scs"), "c", "p")
              posType <- ifelse(type %in% c("scs", "sps"), "cr_s", "dr_s")
              
              # Calculate vsValue based on spread type
              vsValue <- 0
              instrValL <- 0
              instrValH <- 0
              
              if (type %in% c("lcs", "scs")) {
                instrValL <- dte_data[i, callPos]
                instrValH <- dte_data[j, callPos]
                if (type == "lcs") {
                  vsValue <- instrValL - instrValH
                } else {  # scs
                  vsValue <- instrValH - instrValL
                }
              } else {  # lps or sps
                instrValL <- dte_data[i, putPos]
                instrValH <- dte_data[j, putPos]
                if (type == "lps") {
                  vsValue <- instrValH - instrValL
                } else {  # sps
                  vsValue <- instrValL - instrValH
                }
              }
              
              # Calculate p_max based on spread type
              p_max <- 0
              if (type %in% c("lcs", "lps")) {
                p_max <- dist_val - vsValue
              } else {  # scs or sps
                p_max <- vsValue
              }
              
              # Calculate bep based on spread type
              bep <- 0
              if (type %in% c("lcs", "scs")) {
                bep <- strikeL + vsValue
              } else {  # lps or sps
                bep <- strikeH - vsValue
              }
              
              # Calculate moneyness (section 15)
              stockPrice <- dte_data$stockPrice[1]  # Use first stock price for this DTE
              moneyness <- "otm"  # Default
              
              if (strikeL < stockPrice && strikeH > stockPrice) {
                moneyness <- "atm"
              } else if (instrType == "c") {
                if (strikeL < stockPrice && strikeH < stockPrice) {
                  moneyness <- "itm"
                } else if (strikeL > stockPrice && strikeH > stockPrice) {
                  moneyness <- "otm"
                }
              } else if (instrType == "p") {
                if (strikeL > stockPrice && strikeH > stockPrice) {
                  moneyness <- "itm"
                } else if (strikeL < stockPrice && strikeH < stockPrice) {
                  moneyness <- "otm"
                }
              }
              
              # Calculate probability of profit (section 16)
              prob_profit <- 0.5  # Default value
              
              # Try to calculate implied volatility from options prices
              tryCatch({
                # Calculate implied volatility for both legs using Black-Scholes
                T <- days / 365  # Time to expiration in years
                r <- 0.02  # Risk-free rate (2%)
                
                if (type %in% c("lcs", "scs")) {
                  # Calculate IV for call options
                  iv_l <- calculate_implied_volatility(stockPrice, strikeL, T, r, instrValL, "call")
                  iv_h <- calculate_implied_volatility(stockPrice, strikeH, T, r, instrValH, "call")
                } else {
                  # Calculate IV for put options
                  iv_l <- calculate_implied_volatility(stockPrice, strikeL, T, r, instrValL, "put")
                  iv_h <- calculate_implied_volatility(stockPrice, strikeH, T, r, instrValH, "put")
                }
                
                # Average IV from both legs
                iv_avg <- (iv_l + iv_h) / 2
                
                # Calculate probability of profit using normal distribution
                # Following the rules: NORM.DIST(bep, stockPrice, iv * sqrt(dte/365), TRUE)
                time_factor <- sqrt(days / 365)
                if (type %in% c("lcs", "lps")) {
                  # Bull spreads: profit if stock > bep
                  prob_profit <- 1 - pnorm(bep, stockPrice, iv_avg * stockPrice * time_factor)
                } else {
                  # Bear spreads: profit if stock < bep
                  prob_profit <- pnorm(bep, stockPrice, iv_avg * stockPrice * time_factor)
                }
                
                # Ensure probability is between 0 and 1
                prob_profit <- max(0, min(1, prob_profit))
                
              }, error = function(e) {
                # If IV calculation fails, use default probability
                prob_profit <- 0.5
              })
              
              new_row <- data.frame(
                dte = days,
                vsType = type,
                vsValue = vsValue,
                instrValL = instrValL,
                instrValH = instrValH,
                strikeL = strikeL,
                strikeH = strikeH,
                dist = dist_val,
                instrType = instrType,
                posType = posType,
                p_max = p_max,
                bep = bep,
                moneyness = moneyness,
                prob_profit = prob_profit,
                stockPrice = stockPrice,
                updated = Sys.time(),
                strat_type = "vs"
              )
              
              result <- rbind(result, new_row)
            }
          }
        }
      }
    }
  }
  
  if (nrow(result) == 0) {
    warning("No valid vertical spreads found")
    return(list(optionsData = optionsData, vsData = NULL))
  }
  
  # Convert numeric columns
  numeric_cols <- c("vsValue", "instrValL", "instrValH", 
                   "strikeL", "strikeH", "dist", "p_max", "bep", "prob_profit", "stockPrice")
  for (col in numeric_cols) {
    result[[col]] <- as.numeric(result[[col]])
  }
  
  # Save to CSV if output_file is specified
  if (!is.null(output_file)) {
    tryCatch({
      write.csv(result, output_file, row.names = FALSE)
      message(sprintf("Results saved to %s", output_file))
    }, error = function(e) {
      warning(sprintf("Failed to save results to %s: %s", output_file, e$message))
    })
  }
  
  # Return named list with original data and results
  return(list(
    optionsData = optionsData,
    vsData = result
  ))
} 